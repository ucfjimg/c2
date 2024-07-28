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
    TOK_LOGNOT = '!',
    TOK_ASSIGN = '=',
    TOK_QUESTION = '?',
    TOK_COLON = ':',
    TOK_COMMA = ',',

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
    TOK_LOGAND,             // &&
    TOK_LOGOR,              // ||
    TOK_EQUALITY,           // ==
    TOK_NOTEQUAL,           // !=
    TOK_LESSEQUAL,          // <=
    TOK_GREATEREQUAL,       // >=

    //
    // compound assignment
    //
    TOK_COMPOUND_ADD,       // +=
    TOK_COMPOUND_SUBTRACT,  // -=
    TOK_COMPOUND_MULTIPLY,  // *=
    TOK_COMPOUND_DIVIDE,    // /=
    TOK_COMPOUND_MODULO,    // %=
    TOK_COMPOUND_BITAND,    // &=
    TOK_COMPOUND_BITOR,     // |=
    TOK_COMPOUND_BITXOR,    // ^=
    TOK_COMPOUND_LSHIFT,    // <<=
    TOK_COMPOUND_RSHIFT,    // >>=

    //
    // keywords
    //
    TOK_INT,
    TOK_RETURN,
    TOK_VOID,
    TOK_IF,
    TOK_ELSE,
    TOK_GOTO,
    TOK_DO,
    TOK_WHILE,
    TOK_FOR,
    TOK_BREAK,
    TOK_CONTINUE,
    TOK_SWITCH,
    TOK_CASE,
    TOK_DEFAULT,
    TOK_STATIC,
    TOK_EXTERN
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
extern char *token_type_describe(TokenType tt);
extern void token_clone(Token *src, Token *dst);
