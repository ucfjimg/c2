#pragma once

#include "fileline.h"

#include <stdbool.h>
#include <stdlib.h>

typedef enum {
    //
    // operators/puncuation
    //
    TOK_LBRACE = '{',    
    TOK_LPAREN = '(',
    TOK_LBRACKET = '[',
    TOK_RBRACE = '}',
    TOK_RPAREN = ')',
    TOK_RBRACKET = ']',
    TOK_SEMI = ';',
    TOK_PLUS = '+',
    TOK_MINUS = '-',
    TOK_MULTIPLY = '*',
    TOK_DEREF = '*',
    TOK_DIVIDE = '/',
    TOK_MODULO = '%',
    TOK_COMPLEMENT = '~',
    TOK_LESSTHAN = '<',
    TOK_GREATERTHAN = '>',
    TOK_BITAND = '&',
    TOK_ADDROF = '&',
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
    TOK_FLOAT_CONST,
    TOK_STR_CONST,

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
    TOK_EXTERN,
    TOK_LONG,
    TOK_SIGNED,
    TOK_UNSIGNED,
    TOK_DOUBLE,
    TOK_CHAR,
} TokenType;

typedef struct {
    unsigned long intval;       // the bit pattern
    bool is_char;               // a character constant
    bool is_long;               // a long constant
    bool is_unsigned;           // an unsigned constant
} TokIntConst;

typedef struct {
    char *data;                 // string data, unescaped
    size_t length;              // length, including trailing \0
} TokStrConst;

typedef struct {
    TokenType type;             // type for discrimated union
    FileLine loc;               // file/line of first character of token

    union {
        char *id;               // TOK_ID
        TokIntConst int_const;  // TOK_INT_CONST
        double float_const;     // TOK_FLOAT_CONST
        TokStrConst str_const;  // TOK_STR_CONST
        char *err;              // TOK_ERROR, the invalid token
    };
} Token;

extern void token_free(Token *tok);
extern char *token_describe(Token *tok);
extern char *token_type_describe(TokenType tt);
extern void token_clone(Token *src, Token *dst);

