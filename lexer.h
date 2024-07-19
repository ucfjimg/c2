#pragma once

#include "list.h"
#include "token.h"

#include <stdbool.h>
#include <stdio.h>

typedef struct LexerFile {
    ListNode list;              // next file in list
    char *fname;                // filename of this file
} LexerFile;

typedef struct {
    FILE *src;                  // opened source file
    bool line_start;            // true if `ch` is first character on line
    bool end_of_file;           // true if end of file has been reached
    int ch;                     // the next character to lex
    int line;                   // the line containing `ch`
    List files;                 // list of filenames seen so far
} Lexer;

extern Lexer *lexer_open(char *fname);
extern void lexer_close(Lexer *lex);
extern void lexer_token(Lexer *lex, Token *tok);

