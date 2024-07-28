#include "bitmath.h"

#include "ice.h"

//
// Check that the given number is a power of two.
//
bool is_power_of_two(unsigned long val)
{
    return (val & (val - 1)) == 0;
}

//
// Round a given value up to a power of two, so the result
// is divisible by divisor.
//
int align_up(int val, int divisor)
{
    ICE_ASSERT(divisor != 0);
    ICE_ASSERT(is_power_of_two(divisor));

    return (val + divisor - 1) & ~(divisor - 1);
}