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
        case TOK_EOF:           return saprintf("<eof>");
        case TOK_ERROR:         return saprintf("<invalid-token-%s>", tok->err);

        case TOK_ID:            return saprintf("id(%s)", tok->id);
        case TOK_INT_CONST:     return saprintf("int-const(%lu)", tok->intval);

        case TOK_INCREMENT:     return saprintf("++");
        case TOK_DECREMENT:     return saprintf("--");
        
        case TOK_INT:           return saprintf("int");
        case TOK_RETURN:        return saprintf("return");
        case TOK_VOID:          return saprintf("void");

        default:
            break;
    }

    return saprintf("<unknown-token-type-%d>", tok->type);
}
