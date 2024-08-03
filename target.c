#include "target.h"

#include <stdint.h>

#define S2_MIN  ((int16_t)0x8000)
#define S2_MAX  ((int16_t)0x7fff)

#define S4_MIN  ((int32_t)0x80000000)
#define S4_MAX  ((int32_t)0x7fffffff)

#define S8_MIN  ((int64_t)0x8000000000000000ll)
#define S8_MAX  ((int64_t)0x7fffffffffffffffll)

//
// Check if the bit pattern in val represents a signed value
// which will fit into a target integer.
//
bool tgt_signed_fits_in_int(uint64_t val)
{
    int64_t sval = (int64_t)val;
    
#if TARGET_INT_SIZE == 2
    return sval >= S2_MIN && sval <= S2_MAX;
#elif TARGET_INT_SIZE == 4
    return sval >= S4_MIN && sval <= S4_MAX;
#elif TARGET_INT_SIZE == 8
    return sval >= S8_MIN && sval <= S8_MAX;
#else
# error invalid TARGET_INT_SIZE
#endif
}

//
// Check if the bit pattern in val represents a signed value
// which will fit into a target long.
//
bool tgt_signed_fits_in_long(uint64_t val)
{
    int64_t sval = (int64_t)val;
    
#if TARGET_LONG_SIZE == 2
    return sval >= S2_MIN && sval <= S2_MAX;
#elif TARGET_LONG_SIZE == 4
    return sval >= S4_MIN && sval <= S4_MAX;
#elif TARGET_LONG_SIZE == 8
    return sval >= S8_MIN && sval <= S8_MAX;
#else
# error invalid TARGET_INT_SIZE
#endif
}

