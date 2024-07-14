#include "strbuilder.h"

#include "safemem.h"

#define STB_DEFAULT_ALLOC 16

//
// Initialize a string builder to empty, overwriting anything
// that might be in it already.
//
static void stb_init(StrBuilder *stb)
{
    stb->alloc = STB_DEFAULT_ALLOC;
    stb->length = 0;
    stb->str = safe_malloc(stb->alloc);
    stb->str[0] = '\0';
}

//
// Expand, if needed, the allocated space in a string builder, 
// to include at least `extra` new characters of space. An extra byte 
// will be left for the training nul. 
//
static void stb_ensure(StrBuilder *stb, size_t extra)
{
    size_t needed = stb->length + extra + 1;
    if (needed <= stb->alloc) {
        return;
    }

    stb->alloc *= 2;
    if (stb->alloc < needed) {
        stb->alloc = needed;
    }

    stb->str = safe_realloc(stb->str, stb->alloc);
}

//
// Initialize an empty string builder
//
StrBuilder *stb_alloc(void)
{
    StrBuilder *stb = safe_malloc(sizeof(StrBuilder));

    stb_init(stb);

    return stb;
}

//
// Free all memory associated with a string builder
//
void stb_free(StrBuilder *stb)
{
    safe_free(stb->str);
    safe_free(stb);
}

//
// Take ownership (and return) the contained string.
// After this, the string builder is empty.
//
char *stb_take(StrBuilder *stb)
{
    char *str = stb->str;

    stb_init(stb);

    return str;
}

//
// 
//
void stb_push_char(StrBuilder *stb, char ch)
{
    stb_ensure(stb, sizeof(char));

    stb->str[stb->length++] = ch;
    stb->str[stb->length] = '\0';
}
