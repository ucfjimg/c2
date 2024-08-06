#include "constant.h"

//
// Construct in place an integral constant.
//
void const_make_int(Const *cn, ConstIntSize size, ConstIntSign sign, unsigned long value)
{
    cn->tag = CON_INTEGRAL;
    cn->intval.size = size;
    cn->intval.sign = sign;
    cn->intval.value = value;
}

//
// Construct in place a floating point constant.
//
void const_make_double(Const *cn, double value)
{
    cn->tag = CON_FLOAT;
    cn->floatval = value;
}
