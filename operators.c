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
 
