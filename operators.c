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
        case UOP_LOGNOT:        return "!";
    }

    return "<invalid-unary-op>";
}
 
//
// Return a static string describing a binary operator.
//
const char *bop_describe(BinaryOp bop)
{
    switch (bop) {
        case BOP_ADD:           return "+";
        case BOP_SUBTRACT:      return "-";
        case BOP_MULTIPLY:      return "*";
        case BOP_DIVIDE:        return "/";
        case BOP_MODULO:        return "%";
        case BOP_LSHIFT:        return "<<";
        case BOP_RSHIFT:        return ">>";
        case BOP_BITAND:        return "&";
        case BOP_BITOR:         return "|";
        case BOP_BITXOR:        return "^";
        case BOP_LOGAND:        return "&&";
        case BOP_LOGOR:         return "||";
        case BOP_EQUALITY:      return "==";
        case BOP_NOTEQUAL:      return "!=";
        case BOP_LESSTHAN:      return "<";
        case BOP_GREATERTHAN:   return ">";
        case BOP_LESSEQUAL:     return "<=";
        case BOP_GREATEREQUAL:  return ">=";
    }

    return "<invalid-binary-op>";
}
 
//
// Return true if the binary operator is relational.
//
extern bool bop_is_relational(BinaryOp bop)
{
    return bop >= BOP_FIRST_RELATIONAL && bop <= BOP_LAST_RELATIONAL;
}
