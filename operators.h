#pragma once

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
    BOP_EQUALITY,
    BOP_NOTEQUAL,
    BOP_LESSTHAN,
    BOP_GREATERTHAN,
    BOP_LESSEQUAL,
    BOP_GREATEREQUAL,
} BinaryOp;

extern const char *uop_describe(UnaryOp uop);
extern const char *bop_describe(BinaryOp bop);
