#include "operators.h"

//
// Return a static string describing a unary operator.
//
const char *uop_describe(UnaryOp uop)
{
    switch (uop) {
        case UOP_MINUS:         return "-";
        case UOP_PLUS:          return "+";
        case UOP_COMPLEMENT:    return "~";
    }

    return "<invalid-unary-op>";
}
 
//
// Return a static string describing a binary operator.
//
const char *bop_describe(BinaryOp bop)
{
    switch (bop) {
        case BOP_ADD:       return "+";
        case BOP_SUBTRACT:  return "-";
        case BOP_MULTIPLY:  return "*";
        case BOP_DIVIDE:    return "/";
        case BOP_MODULO:    return "%";
    }

    return "<invalid-binary-op>";
}
 
