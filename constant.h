#pragma once

#include <stdbool.h>

//
// A constant value can only hold a small subset of types.
//
typedef enum {
    CON_INTEGRAL,
    CON_FLOAT,
} ConstTag;

typedef enum {
    CIS_INT,
    CIS_LONG,
} ConstIntSize;

typedef enum {
    CIS_SIGNED,
    CIS_UNSIGNED,
} ConstIntSign;

typedef struct {
    ConstIntSize        size;
    ConstIntSign        sign;
    unsigned long       value;
} ConstInt;

typedef struct {
    ConstTag tag;

    union {
        ConstInt        intval;
        double          floatval;
    };
} Const;

extern Const const_make_int(ConstIntSize size, ConstIntSign sign, unsigned long value);
extern Const const_make_zero(void);
extern void const_make_double(Const *cn, double value);
extern char *const_describe(Const *cn);
extern bool const_unsigned(Const *cn);
