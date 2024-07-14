#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "safemem.h"

//
// Allocate a block of memory, and exit the program on error.
//
void *safe_malloc(size_t len)
{
    void *blk = malloc(len);
    if (!blk) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    return blk;
}

//
// Allocate a zeroed block of memory, and exit the program on error.
//
void *safe_zalloc(size_t len)
{
    void *blk = malloc(len);
    if (!blk) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }

    memset(blk, 0, len);
    
    return blk;
}

//
// Reallocate a block of memory, and exit the program on error.
// Semantics are the same as realloc()
//
void *safe_realloc(void *p, size_t len)
{
    p = realloc(p, len);
    if (!p) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    return p;
}

//
// Return an allocated copy of a string, else exit the program on out of memory.
//
char *safe_strdup(char *str)
{
    str = strdup(str);
    if (!str) {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    return str;
}

//
// Free a pointer allocated by any of the safe allocation functions.
//
void safe_free(void *p)
{
    free(p);
}

//
// sprintf format the arguments and return the result as an allocated string.
// Exit the program on out of memory.
// 
char *saprintf(char *fmt, ...)
{
    char ch;
    char *str;
    size_t n;
    va_list arg;
    
    va_start(arg, fmt);
    n = vsnprintf(&ch, 1, fmt, arg);
    va_end(arg);

    str = safe_malloc(n + 1);

    va_start(arg, fmt);
    vsnprintf(str, n+1, fmt, arg);
    va_end(arg);

    return str;
}

