#include "strutil.h"

#include "safemem.h"

#include <string.h>>

//
// Return an allocated string containing `count` contiguous `ch` 
// characters.
//
char *str_repeat(int count, char ch)
{
    char *str = safe_malloc(count + 1);

    memset(str, ch, count);
    str[count] = '\0';

    return str;
}
