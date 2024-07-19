#pragma once

extern void ice_handle(const char *cond, const char *file, int line);

//
// Internal Compiler Error - assert for compiler bugs.
//
#define ICE_ASSERT(cond)                            \
    do {                                            \
        if (!(cond)) {                              \
            ice_handle(#cond, __FILE__, __LINE__);  \
        }                                           \
    } while(0);
