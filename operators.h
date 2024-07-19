#pragma once

typedef enum {
    UOP_PLUS,
    UOP_MINUS,
    UOP_COMPLEMENT,
} UnaryOp;

extern const char *uop_describe(UnaryOp uop);
