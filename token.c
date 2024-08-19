#include "token.h"

#include "ice.h"
#include "safemem.h"
#include "strbuilder.h"

#include <ctype.h>

//
// Free any memory associated with a token. Note that the token itself is 
// not an allocated object, only the pointers within it.
//
void token_free(Token *tok)
{
    switch (tok->type) {
        case TOK_ID:
            safe_free(tok->id);
            break;

        case TOK_ERROR:
            safe_free(tok->err);
            break;

        case TOK_STR_CONST:
            safe_free(tok->str_const.data);
            break;

        default:
            break;
    }
}

//
// Format an integer constant into an allocated string.
//
static char *token_describe_int_const(Token *tok)
{
    ICE_ASSERT(tok->type == TOK_INT_CONST);

    return saprintf("int-const(%lu%s)",
        tok->int_const.intval,
        tok->int_const.is_long ? "l" : "");
}

//
// Format a float constant into an allocated string.
//
static char *token_describe_float_const(Token *tok)
{
    ICE_ASSERT(tok->type == TOK_FLOAT_CONST);

    return saprintf("float-const(%g)", tok->float_const);
}

//
// Format a string constant into an allocated string.
//
static char *token_describe_str_const(Token *tok)
{
    ICE_ASSERT(tok->type == TOK_STR_CONST);
    TokStrConst *str = &tok->str_const;

    StrBuilder *stb = stb_alloc();

    for (size_t i = 0; i < str->length; i++) {
        char ch = str->data[i];

        if (isprint(ch)) {
            stb_push_char(stb, ch);
        } else {
            switch (ch) {
                case '\0':  stb_printf(stb, "\\0"); break;
                case '\a':  stb_printf(stb, "\\a"); break;
                case '\b':  stb_printf(stb, "\\b"); break;
                case '\f':  stb_printf(stb, "\\f"); break;
                case '\n':  stb_printf(stb, "\\n"); break;
                case '\r':  stb_printf(stb, "\\r"); break;
                case '\t':  stb_printf(stb, "\\t"); break;
                case '\v':  stb_printf(stb, "\\v"); break;

                default:
                    stb_printf(stb, "\\x%02x", ch & 0xff);
                    break;
            }
        }
    }

    char *desc = stb_take(stb);
    stb_free(stb);
    return desc;
} 

//
// Return a description of a token for debug output or error message. 
// Returns an allocated string which must be free'd.
//
char *token_describe(Token *tok)
{
    if (tok->type < TOK_FENCE) {
        //
        // Single character ASCII token
        //
        return saprintf("%c", tok->type);
    }

    switch (tok->type) {
        case TOK_EOF:               return saprintf("<eof>");
        case TOK_ERROR:             return saprintf("<invalid-token-%s>", tok->err);

        case TOK_ID:                return saprintf("id(%s)", tok->id);
        case TOK_INT_CONST:         return token_describe_int_const(tok); break;
        case TOK_FLOAT_CONST:       return token_describe_float_const(tok); break;
        case TOK_STR_CONST:         return token_describe_str_const(tok); break;

        default:                    break;
    }

    return token_type_describe(tok->type);
}

//
// Return a description of a token type for debug output or error message. 
// Returns an allocated string which must be free'd.
//
char *token_type_describe(TokenType tt)
{
    if (tt < TOK_FENCE) {
        //
        // Single character ASCII token
        //
        return saprintf("%c", tt);
    }

    switch (tt) {
        case TOK_EOF:               return saprintf("<eof>");
        case TOK_ERROR:             return saprintf("<invalid-token>");

        case TOK_ID:                return saprintf("<id>");
        case TOK_INT_CONST:         return saprintf("<int-const>");
        case TOK_FLOAT_CONST:       return saprintf("<float-const>");
        case TOK_STR_CONST:         return saprintf("<string-const>");

        case TOK_INCREMENT:         return saprintf("++");
        case TOK_DECREMENT:         return saprintf("--");
        case TOK_LSHIFT:            return saprintf("<<");
        case TOK_RSHIFT:            return saprintf(">>");
        case TOK_LOGAND:            return saprintf("&&");
        case TOK_LOGOR:             return saprintf("||");
        case TOK_EQUALITY:          return saprintf("==");
        case TOK_NOTEQUAL:          return saprintf("!=");
        case TOK_LESSEQUAL:         return saprintf("<=");
        case TOK_GREATEREQUAL:      return saprintf(">=");

        case TOK_COMPOUND_ADD:      return saprintf("+=");
        case TOK_COMPOUND_SUBTRACT: return saprintf("-=");
        case TOK_COMPOUND_MULTIPLY: return saprintf("*=");
        case TOK_COMPOUND_DIVIDE:   return saprintf("/=");
        case TOK_COMPOUND_MODULO:   return saprintf("%%=");
        case TOK_COMPOUND_BITAND:   return saprintf("&=");
        case TOK_COMPOUND_BITOR:    return saprintf("|=");
        case TOK_COMPOUND_BITXOR:   return saprintf("^=");
        case TOK_COMPOUND_LSHIFT:   return saprintf("<<=");
        case TOK_COMPOUND_RSHIFT:   return saprintf(">>=");

        case TOK_INT:               return saprintf("int");
        case TOK_LONG:              return saprintf("long");
        case TOK_RETURN:            return saprintf("return");
        case TOK_VOID:              return saprintf("void");
        case TOK_IF:                return saprintf("if");
        case TOK_ELSE:              return saprintf("else");
        case TOK_GOTO:              return saprintf("goto");
        case TOK_DO:                return saprintf("do");
        case TOK_WHILE:             return saprintf("while");
        case TOK_FOR:               return saprintf("for");
        case TOK_BREAK:             return saprintf("break");
        case TOK_CONTINUE:          return saprintf("continue");
        case TOK_SWITCH:            return saprintf("switch");
        case TOK_CASE:              return saprintf("case");
        case TOK_DEFAULT:           return saprintf("default");
        case TOK_STATIC:            return saprintf("static");
        case TOK_EXTERN:            return saprintf("extern");
        case TOK_SIGNED:            return saprintf("signed");
        case TOK_UNSIGNED:          return saprintf("unsigned");
        case TOK_DOUBLE:            return saprintf("double");
        case TOK_CHAR:              return saprintf("char");

        //
        // All of these are handled by the single-character case.
        //
        case TOK_FENCE:
        case TOK_LBRACE:
        case TOK_LPAREN:
        case TOK_LBRACKET:
        case TOK_RBRACE:
        case TOK_RPAREN:
        case TOK_RBRACKET:
        case TOK_SEMI:
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_MULTIPLY:
        case TOK_DIVIDE:
        case TOK_MODULO:
        case TOK_COMPLEMENT:
        case TOK_LESSTHAN:
        case TOK_GREATERTHAN:
        case TOK_BITAND:
        case TOK_BITOR:
        case TOK_BITXOR:
        case TOK_LOGNOT:
        case TOK_ASSIGN:
        case TOK_QUESTION:
        case TOK_COLON:
        case TOK_COMMA:
            break;

    }

    return saprintf("<invalid-token-type-%d>", tt);
}

//
// Create a copy of the token in `src` into `dst`, including all 
// allocated memory.
//
void token_clone(Token *src, Token *dst)
{
    *dst = *src;

    switch (dst->type) {
        case TOK_ID:
            dst->id = safe_strdup(src->id);
            break;

        case TOK_ERROR:
            dst->err = safe_strdup(src->id);
            break;

        case TOK_STR_CONST:
            dst->str_const.data = safe_malloc(src->str_const.length);
            memcpy(dst->str_const.data, src->str_const.data, src->str_const.length);
            break;

        default:
            break;
    }
}
