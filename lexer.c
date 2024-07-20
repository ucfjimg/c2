#include "lexer.h"

#include "errors.h"
#include "safemem.h"
#include "strbuilder.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

typedef struct {
    char *text;             // text of the keyword
    TokenType tok_type;     // associated token type
} Keyword;

static Keyword keywords[] = {
    { "int",    TOK_INT },
    { "return", TOK_RETURN },
    { "void",   TOK_VOID },

    { NULL,     TOK_EOF }
};

//
// Push a new filename on the end of the filename list. Filenames are
// allocated and kept in scope for the entire lifetime of the lexer as
// they are propogated to future passes for use in error messages.
// 
static void lexer_push_filename(Lexer *lex, char *fname)
{
    LexerFile *file = safe_malloc(sizeof(LexerFile));
    file->fname = fname;
    list_push_back(&lex->files, &file->list);
}

//
// Return the current source file.
//
static char *lexer_current_file(Lexer *lex)
{
    return CONTAINER_OF(lex->files.tail, LexerFile, list)->fname;
}

//
// Handle a preprocesser outline line, which indicates the file and line 
// number of the following line.
//
static void lexer_preproc_line(Lexer *lex)
{
    int line;

    //
    // lines are of the form 
    // # linenum "filename" flag [flag ...]
    //

    fscanf(lex->src, "%d", &line);
    
    //
    // Skip for the leading quote of the filename
    //
    while (true) {
        lex->ch = fgetc(lex->src);
        if (lex->ch == EOF) {
            lex->end_of_file = true;
            return;
        }

        if (lex->ch == '\n') {
            //
            // premature end of file
            //
            lex->line++;
            return;
        }

        if (lex->ch == '"') {
            break;
        }
    }

    long name_start = ftell(lex->src);
    
    //
    // Skip for the ending quote of the filename
    //
    while (true) {
        lex->ch = fgetc(lex->src);
        if (lex->ch == EOF) {
            lex->end_of_file = true;
            return;
        }

        if (lex->ch == '\n') {
            //
            // premature end of file
            //
            lex->line++;
            return;
        }

        if (lex->ch == '"') {
            break;
        }
    }

    long name_end = ftell(lex->src);
    long name_len = name_end - name_start - 1;

    char *name = safe_malloc(name_len + 1);
    fseek(lex->src, name_start, SEEK_SET);
    fread(name, 1, name_len, lex->src);
    name[name_len] = '\0';
    
    lexer_push_filename(lex, name);
    lex->line = line;

    //
    // Skip the rest of the line
    //
    while (true) {
        lex->ch = fgetc(lex->src);
        if (lex->ch == EOF) {
            lex->end_of_file = true;
            return;
        }

        if (lex->ch == '\n') {
            //
            // premature end of file
            //
            break;
        }
    }
}

//
// Return the next character in the stream, handline preprocessor
// directives and counting lines. The current character will be left
// in `lex->ch`.
//
static void lexer_next_char(Lexer *lex)
{
    while (true) {
        if (lex->end_of_file) {
            return;
        } 

        lex->ch = fgetc(lex->src);
        if (lex->ch == EOF) {
            lex->end_of_file = true;
            return;
        }

        if (lex->ch == '#' && lex->line_start) {
            lexer_preproc_line(lex);
            continue;
        }

        if (lex->ch == '\n') {
            lex->line++;
            lex->line_start = true;
        } else {
            lex->line_start = false;
        }

        break;
    }
}

//
// Scan an identifer and return it in `tok`. If the identifier is a 
// keyword, return the keyword
//
static void lexer_scan_id_or_keyword(Lexer *lex, Token *tok)
{
    StrBuilder *stb = stb_alloc();

    do {
        stb_push_char(stb, lex->ch);
        lexer_next_char(lex);
    } while (isalnum(lex->ch) || lex->ch == '_');

    TokenType keyword = TOK_ERROR;

    for (int i = 0; keywords[i].text != NULL; i++) {
        if (strcmp(stb->str, keywords[i].text) == 0) {
            keyword = keywords[i].tok_type;
            break;
        }
    }

    if (keyword != TOK_ERROR) {
        tok->type = keyword;
    } else {
        tok->type = TOK_ID;
        tok->id = stb_take(stb);
    }

    stb_free(stb);
}

//
// Scan an integer constant
//
static void lexer_scan_int_const(Lexer *lex, Token *tok)
{
    StrBuilder *integer = stb_alloc();
    
    do {
        stb_push_char(integer, lex->ch);
        lexer_next_char(lex);
    } while (isdigit(lex->ch));

    //
    // digits cannot be immediately followed by an identifier without intervening
    // whitespace or an operator
    //
    if (isalpha(lex->ch) || lex->ch == '_') {
        do {
            stb_push_char(integer, lex->ch);
            lexer_next_char(lex);
        } while (isalnum(lex->ch) || lex->ch == '_');

        tok->type = TOK_ERROR;
        tok->err = stb_take(integer);

        err_report(EC_ERROR, &tok->loc, "invalid token `%s`", tok->err);
    } else {
        unsigned long val = strtoul(integer->str, NULL, 10);
        
        tok->type = TOK_INT_CONST;
        tok->intval = val;
    }

    stb_free(integer);
}

//
// Scan for multi-character operators, and set the token appropriately.
// Returns true if a token was found, else false.
//
static bool lexer_scan_multichar_op(Lexer *lex, Token *tok)
{
    if (lex->ch == '-') {
        lexer_next_char(lex);
        if (lex->ch == '-') {
            lexer_next_char(lex);
            tok->type = TOK_DECREMENT;
        } else {
            tok->type = TOK_MINUS;
        }

        return true;
    }

    if (lex->ch == '+') {
        lexer_next_char(lex);
        if (lex->ch == '+') {
            lexer_next_char(lex);
            tok->type = TOK_INCREMENT;
        } else {
            tok->type = TOK_PLUS;
        }

        return true;
    }

    if (lex->ch == '<') {
        lexer_next_char(lex);
        if (lex->ch == '<') {
            lexer_next_char(lex);
            tok->type = TOK_LSHIFT;
        } else {
            tok->type = TOK_LESSTHAN;
        }

        return true;
    }

    if (lex->ch == '>') {
        lexer_next_char(lex);
        if (lex->ch == '>') {
            lexer_next_char(lex);
            tok->type = TOK_RSHIFT;
        } else {
            tok->type = TOK_GREATERTHAN;
        }

        return true;
    }

    return false;
}

//
// Allocate and initialize a Lexer object.
//
Lexer *lexer_open(char *fname)
{
    Lexer *lex = safe_malloc(sizeof(Lexer));
    
    lex->src = fopen(fname, "rb");
    if (!lex->src) {
        perror(fname);
        return NULL;
    }

    lex->line_start = true;
    lex->end_of_file = false;
    lex->line = 1;
    list_clear(&lex->files);

    lexer_push_filename(lex, safe_strdup(fname));
    lexer_next_char(lex);

    return lex;
}

//
// Close the lexer file and free the lexer.
//
void lexer_close(Lexer *lex)
{
    if (lex) {
        for (ListNode *curr = lex->files.head; curr; ) {
            ListNode *next = curr->next;
            LexerFile *file = CONTAINER_OF(curr, LexerFile, list);
            safe_free(file->fname);
            safe_free(file);
            curr = next;
        }
    
        fclose(lex->src);
        safe_free(lex);
    }
}

//
// Scan the next token and store it in `tok`.
//
void lexer_token(Lexer *lex, Token *tok)
{
    while (isspace(lex->ch)) {
        lexer_next_char(lex);
    }

    if (lex->end_of_file) {
        tok->type = TOK_EOF;
        return;
    }

    tok->type = TOK_ERROR;
    tok->loc.fname = lexer_current_file(lex);
    tok->loc.line = lex->line;

    switch (lex->ch) {
        case '{': tok->type = TOK_LBRACE; break;
        case '}': tok->type = TOK_RBRACE; break;
        case '(': tok->type = TOK_LPAREN; break;
        case ')': tok->type = TOK_RPAREN; break;
        case ';': tok->type = TOK_SEMI; break;
        case '~': tok->type = TOK_COMPLEMENT; break;
        case '*': tok->type = TOK_MULTIPLY; break;
        case '/': tok->type = TOK_DIVIDE; break;
        case '%': tok->type = TOK_MODULO; break;
    }

    if (tok->type != TOK_ERROR) {
        lexer_next_char(lex);
        return;
    }

    if (lexer_scan_multichar_op(lex, tok)) {
        return;
    }

    if (isalpha(lex->ch) || lex->ch == '_') {
        lexer_scan_id_or_keyword(lex, tok);
        return;
    }

    if (isdigit(lex->ch)) {
        lexer_scan_int_const(lex, tok);
        return;
    }

    tok->type = TOK_ERROR;
    if (isprint(lex->ch)) {
        tok->err = saprintf("char-%c", lex->ch);
    } else {
        tok->err = saprintf("char-%02x", lex->ch & 0xff);
    }

    err_report(EC_ERROR, &tok->loc, "invalid token `%s`", tok->err);

    lexer_next_char(lex);
}
