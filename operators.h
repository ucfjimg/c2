#pragma once

#include <stdbool.h>

typedef enum {
    UOP_PLUS,
    UOP_MINUS,
    UOP_COMPLEMENT,
    UOP_LOGNOT,
} UnaryOp;

typedef enum {
    BOP_ADD,
    BOP_SUBTRACT,
    BOP_MULTIPLY,
    BOP_DIVIDE,
    BOP_MODULO,
    BOP_LSHIFT,
    BOP_RSHIFT,
    BOP_BITAND,
    BOP_BITOR,
    BOP_BITXOR,
    BOP_LOGAND,
    BOP_LOGOR,

    BOP_FIRST_RELATIONAL,
    BOP_EQUALITY = BOP_FIRST_RELATIONAL,
    BOP_NOTEQUAL,
    BOP_LESSTHAN,
    BOP_GREATERTHAN,
    BOP_LESSEQUAL,
    BOP_GREATEREQUAL,
    BOP_LAST_RELATIONAL = BOP_GREATEREQUAL,

} BinaryOp;

extern bool bop_is_relational(BinaryOp bop);
extern const char *uop_describe(UnaryOp uop);
extern const char *bop_describe(BinaryOp bop);
