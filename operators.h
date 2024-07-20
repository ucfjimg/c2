#pragma once

typedef enum {
    UOP_PLUS,
    UOP_MINUS,
    UOP_COMPLEMENT,
} UnaryOp;

typedef enum {
    BOP_ADD,
    BOP_SUBTRACT,
    BOP_MULTIPLY,
    BOP_DIVIDE,
    BOP_MODULO,
    BOP_LSHIFT,
    BOP_RSHIFT,
} BinaryOp;

extern const char *uop_describe(UnaryOp uop);
extern const char *bop_describe(BinaryOp bop);
