#include "temporary.h"

#include "safemem.h"

#include <stdarg.h>

//
// Allocate an id to use in a name of any format to make a program-unique
// label. If the id is later used in tmp_id_name, the label will be unique
// amongst all labels from tmp_id_name and tmp_name.
//
int tmp_alloc_id(void)
{
    static int id = 0;
    return id++;
}

//
// Create a program-wide unique name from a base and and id returned 
// from tmp_alloc_id. The returned string is allocated.
//
char *tmp_id_name(char *base, int id)
{
    return saprintf("%s..%d", base, id);
}

//
// Create a program-wide unique name, which is valid for the assembler
// but is not valid as an identifier in the program itself (i.e. cannot
// collide with any user-defined name). The returned string is allocated.
//
char *tmp_name(char *base) 
{
    return tmp_id_name(base, tmp_alloc_id());
}
