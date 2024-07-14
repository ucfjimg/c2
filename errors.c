#include "errors.h"
#include "safemem.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static int error_count = 0;

//
// Report an error to stderr. If the error is fatal, immediately exit 
// with non-zero status. Otherwise, print the error and count errors.
// 
// if `loc` is not NULL, the error will be prefixed with the file and line.
//
void err_report(ErrorClass ec, FileLine *loc, char *format, ...)
{
    if (loc != NULL) {
        char *fileline = fileline_describe(loc);
        fprintf(stderr, "%s: ", fileline);
        safe_free(fileline);
    }

    switch (ec) {
        case EC_FATAL: 
            fprintf(stderr, "fatal: "); 
            break;
    
        case EC_ERROR: 
            fprintf(stderr, "error: "); 
            error_count++;
            break;
    
        case EC_WARNING: 
            fprintf(stderr, "warning: "); 
            break;
    }

    va_list arg;
    va_start(arg, format);
    vfprintf(stderr, format, arg);
    va_end(arg);

    fprintf(stderr, "\n");

    if (ec == EC_FATAL) {
        exit(2);
    }
}

//
// Return true if there were any errors (not warnings).
//
bool err_has_errors(void)
{
    return error_count;
}

