#pragma once

#include <stdbool.h>

typedef enum {
    UOP_PLUS,
    UOP_MINUS,
    UOP_COMPLEMENT,
    UOP_LOGNOT,
    UOP_PREINCREMENT,
    UOP_PREDECREMENT,
    UOP_POSTINCREMENT,
    UOP_POSTDECREMENT,
} UnaryOp;

typedef enum {
    BOP_ADD,
    BOP_SUBTRACT,
    BOP_MULTIPLY,
    BOP_DIVIDE,
    BOP_DIVDBL,
    BOP_MODULO,
    BOP_LSHIFT,
    BOP_RSHIFT,
    BOP_BITAND,
    BOP_BITOR,
    BOP_BITXOR,
    BOP_LOGAND,
    BOP_LOGOR,

    BOP_CONDITIONAL,            // really, ternary, but parsed as binary

    BOP_ASSIGN,
    BOP_FIRST_COMPOUND_ASSIGN,
    BOP_COMPOUND_ADD = BOP_FIRST_COMPOUND_ASSIGN,
    BOP_COMPOUND_SUBTRACT,
    BOP_COMPOUND_MULTIPLY,
    BOP_COMPOUND_DIVIDE,
    BOP_COMPOUND_MODULO,
    BOP_COMPOUND_BITAND,
    BOP_COMPOUND_BITOR,
    BOP_COMPOUND_BITXOR,
    BOP_COMPOUND_LSHIFT,
    BOP_COMPOUND_RSHIFT,
    BOP_LAST_COMPOUND_ASSIGN = BOP_COMPOUND_RSHIFT,

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
extern bool bop_is_compound_assign(BinaryOp bop);
extern BinaryOp bop_compound_to_binop(BinaryOp bop);
extern const char *uop_describe(UnaryOp uop);
extern const char *bop_describe(BinaryOp bop);
