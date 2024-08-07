#include "constant.h"

#include "safemem.h"

//
// Construct an integral constant.
//
Const const_make_int(ConstIntSize size, ConstIntSign sign, unsigned long value)
{
    Const cn;

    cn.tag = CON_INTEGRAL;
    cn.intval.size = size;
    cn.intval.sign = sign;
    cn.intval.value = value;

    return cn;
}

//
// Construct a zero int constant.
//
Const const_make_zero(void)
{
    return const_make_int(CIS_INT, CIS_SIGNED, 0);
}

//
// Construct in place a floating point constant.
//
void const_make_double(Const *cn, double value)
{
    cn->tag = CON_FLOAT;
    cn->floatval = value;
}

//
// Return a description, in an allocated string, of an integral constant.
//
static char *const_describe_int(ConstInt *cn)
{
    return saprintf("%lu%s%s",
        cn->value,
        cn->size == CIS_LONG ? "l" : "",
        cn->sign == CIS_UNSIGNED ? "u" : "");
}

//
// Return a description, in an allocated string, of the given constant.
//
char *const_describe(Const *cn)
{
    switch (cn->tag) {
        case CON_INTEGRAL:      return const_describe_int(&cn->intval);
        case CON_FLOAT:         return saprintf("%g", cn->floatval);
    }

    return saprintf("<invalid-constant>");
}

//
// Return true if the constant represents an unsigned integer of any size.
//
bool const_unsigned(Const *cn)
{
    return cn->tag == CON_INTEGRAL && cn->intval.sign == CIS_UNSIGNED;
}
