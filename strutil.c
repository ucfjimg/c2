#include "strutil.h"

#include "strbuilder.h"

#include <ctype.h>

//
// Given arbitrary data, return a description as an escape C string.
//
char *str_escape(char *data, size_t length)
{
    StrBuilder *stb = stb_alloc();

    for (size_t i = 0; i < length; i++) {
        char ch = data[i];

        if (isprint(ch)) {
            stb_push_char(stb, ch);
        } else {
            switch (ch) {
                case '\0':      stb_printf(stb, "\\0"); break;
                case '\a':      stb_printf(stb, "\\a"); break;
                case '\b':      stb_printf(stb, "\\b"); break;
                case '\f':      stb_printf(stb, "\\f"); break;
                case '\n':      stb_printf(stb, "\\n"); break;
                case '\r':      stb_printf(stb, "\\r"); break;
                case '\t':      stb_printf(stb, "\\t"); break;
                case '\v':      stb_printf(stb, "\\v"); break;
            
                default:
                    stb_printf(stb, "\\x%02x", ch & 0xff);
            }
        }
    }

    char *desc = stb_take(stb);
    stb_free(stb);

    return desc;
}