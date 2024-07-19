#include "ice.h"

#include <stdio.h>
#include <stdlib.h>

//
// Report an internal compiler error and abort immediately. This
// function does not exit.
//
void ice_handle(const char *cond, const char *file, int line)
{
    fprintf(stderr, "internal compiler error: %s@%d: %s\n", file, line, cond);

    fflush(stdout);
    fflush(stderr);
    abort();
}
