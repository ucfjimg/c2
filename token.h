#pragma once

#include "fileline.h"

typedef enum {
    //
    // operators/puncuation
    //
    TOK_LBRACE = '{',    
    TOK_LPAREN = '(',
    TOK_RBRACE = '}',
    TOK_RPAREN = ')',
    TOK_SEMI = ';',
    TOK_PLUS = '+',
    TOK_MINUS = '-',
    TOK_MULTIPLY = '*',
    TOK_DIVIDE = '/',
    TOK_MODULO = '%',
    TOK_COMPLEMENT = '~',
    TOK_LESSTHAN = '<',
    TOK_GREATERTHAN = '>',
    TOK_BITAND = '&',
    TOK_BITOR = '|',
    TOK_BITXOR = '^',

    TOK_FENCE = 256,        // keep rest of tokens from overlapping ASCII

    // 
    // meta tokens
    //
    TOK_EOF,
    TOK_ERROR,

    //
    // values
    //
    TOK_ID,
    TOK_INT_CONST,

    //
    // multi-char operators
    //
    TOK_INCREMENT,          // ++
    TOK_DECREMENT,          // --
    TOK_RSHIFT,             // >>
    TOK_LSHIFT,             // <<

    //
    // keywords
    //
    TOK_INT,
    TOK_RETURN,
    TOK_VOID,

} TokenType;

typedef struct {
    TokenType type;             // type for discrimated union
    FileLine loc;               // file/line of first character of token

    union {
        char *id;               // if TOK_ID
        unsigned long intval;   // if TOK_INT_CONST
        char *err;              // if TOK_ERROR, the invalid token
    };
} Token;

extern void token_free(Token *tok);
extern char *token_describe(Token *tok);
