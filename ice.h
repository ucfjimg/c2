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

//
// ICE for features not yet implemented.
//
#define ICE_NYI(feature)                            \
    do {                                            \
        ice_handle(                                 \
            feature ": not yet implemented.",       \
            __FILE__, __LINE__);                    \
    } while(0);
