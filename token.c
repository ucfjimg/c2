#include "token.h"

#include "safemem.h"

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

        default:
            break;
    }
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
        case TOK_INT_CONST:         return saprintf("int-const(%lu)", tok->intval);

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
        case TOK_RETURN:            return saprintf("return");
        case TOK_VOID:              return saprintf("void");
        case TOK_IF:                return saprintf("if");
        case TOK_ELSE:              return saprintf("else");
        case TOK_GOTO:              return saprintf("goto");

        default:
            break;
    }

    return saprintf("<unknown-token-type-%d>", tok->type);
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

        default:
            break;
    }
}
