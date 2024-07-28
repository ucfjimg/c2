#pragma once

#include <string.h>

typedef struct {
    size_t length;
    size_t alloc;
    char *str;
} StrBuilder;

extern StrBuilder *stb_alloc(void);
extern void stb_free(StrBuilder *stb);
extern char *stb_take(StrBuilder *stb);
extern void stb_push_char(StrBuilder *stb, char ch);
extern void stb_printf(StrBuilder *stb, const char *fmt, ...);

