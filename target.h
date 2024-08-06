#pragma once

#include <stdbool.h>
#include <stdint.h>

//
//  Definitions for type sizes on the compilation target.
//

#define TARGET_INT_SIZE         4
#define TARGET_LONG_SIZE        8

extern bool tgt_signed_fits_in_int(uint64_t val);
extern bool tgt_signed_fits_in_long(uint64_t val);
extern bool tgt_unsigned_fits_in_int(uint64_t val);
extern bool tgt_unsigned_fits_in_long(uint64_t val);



