#include "operators.h"

#include "ice.h"

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
        case UOP_PREINCREMENT:  return "(pre)++";
        case UOP_PREDECREMENT:  return "(pre)--";
        case UOP_POSTINCREMENT: return "(post)++";
        case UOP_POSTDECREMENT: return "(post)--";
        case UOP_SHR:           return ">>1";
    }

    return "<invalid-unary-op>";
}
 
//
// Return a static string describing a binary operator.
//
const char *bop_describe(BinaryOp bop)
{
    switch (bop) {
        case BOP_ADD:               return "+";
        case BOP_SUBTRACT:          return "-";
        case BOP_MULTIPLY:          return "*";
        case BOP_DIVIDE:            return "/";
        case BOP_DIVDBL:            return "/.";
        case BOP_MODULO:            return "%";
        case BOP_LSHIFT:            return "<<";
        case BOP_RSHIFT:            return ">>";
        case BOP_URSHIFT:           return ">>>";
        case BOP_BITAND:            return "&";
        case BOP_BITOR:             return "|";
        case BOP_BITXOR:            return "^";
        case BOP_LOGAND:            return "&&";
        case BOP_LOGOR:             return "||";
        case BOP_CONDITIONAL:       return "?:";
        case BOP_ASSIGN:            return "=";
        case BOP_COMPOUND_ADD:      return "+=";
        case BOP_COMPOUND_SUBTRACT: return "-=";
        case BOP_COMPOUND_MULTIPLY: return "*=";
        case BOP_COMPOUND_DIVIDE:   return "/=";
        case BOP_COMPOUND_MODULO:   return "%=";
        case BOP_COMPOUND_BITAND:   return "&=";
        case BOP_COMPOUND_BITOR:    return "|=";
        case BOP_COMPOUND_BITXOR:   return "^=";
        case BOP_COMPOUND_LSHIFT:   return "<<=";
        case BOP_COMPOUND_RSHIFT:   return ">>=";
        case BOP_COMPOUND_URSHIFT:  return ">>>=";
        case BOP_EQUALITY:          return "==";
        case BOP_NOTEQUAL:          return "!=";
        case BOP_LESSTHAN:          return "<";
        case BOP_GREATERTHAN:       return ">";
        case BOP_LESSEQUAL:         return "<=";
        case BOP_GREATEREQUAL:      return ">=";
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

//
// Return true if the binary operator is a compound assignment.
//
bool bop_is_compound_assign(BinaryOp bop)
{
    return bop >= BOP_FIRST_COMPOUND_ASSIGN && bop <= BOP_LAST_COMPOUND_ASSIGN;
}

//
// Convert a compound assignment operator to the underlying binary operator.
//
BinaryOp bop_compound_to_binop(BinaryOp bop)
{
    switch (bop) {
        case BOP_COMPOUND_ADD: return BOP_ADD; 
        case BOP_COMPOUND_SUBTRACT: return BOP_SUBTRACT; 
        case BOP_COMPOUND_MULTIPLY: return BOP_MULTIPLY; 
        case BOP_COMPOUND_DIVIDE: return BOP_DIVIDE; 
        case BOP_COMPOUND_MODULO: return BOP_MODULO; 
        case BOP_COMPOUND_BITAND: return BOP_BITAND; 
        case BOP_COMPOUND_BITOR: return BOP_BITOR; 
        case BOP_COMPOUND_BITXOR: return BOP_BITXOR; 
        case BOP_COMPOUND_LSHIFT: return BOP_LSHIFT; 
        case BOP_COMPOUND_RSHIFT: return BOP_RSHIFT; 
        case BOP_COMPOUND_URSHIFT: return BOP_URSHIFT; 

        default:
            ICE_ASSERT(((void)"invalid compound assignment in bop_compound_to_binop", false));
    }

    return BOP_ADD;
}
