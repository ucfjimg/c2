#include "type.h"

#include "safemem.h"

//
// Allocator for all types.
//
static Type *type_alloc(TypeTag tag)
{
    Type *type = safe_zalloc(sizeof(Type));
    type->tag = tag;
    return type;
}

//
// Constructor for an integer type.
//
Type *type_int(void)
{
    return type_alloc(TT_INT);
}

//
// Constructor for a function type.
//
Type *type_function(int parms)
{
    Type *type = type_alloc(TT_FUNC);
    type->func.parms = parms;
    return type;
}

//
// Free a type.
//
void type_free(Type *type)
{
    safe_free(type);
}

//
// Return an allocated string describing a function type.
//
static char *type_describe_func(TypeFunction *func)
{
    char *desc = NULL;

    if (func->parms == 0) {
        desc = saprintf("int(void)");
    } else {
        desc = saprintf("int(");
        for (int i = 0; i < func->parms - 1; i++) {
            char *newdesc = saprintf("%sint, ", desc);
            safe_free(desc);
            desc = newdesc;
        }

        char *newdesc = saprintf("%sint)", desc);
        safe_free(desc);
        desc = newdesc;
    }

    return desc;
}

//
// Return an allocated string describing a type.
//
char *type_describe(Type *type)
{
    switch (type->tag) {
        case TT_INT:    return saprintf("int");
        case TT_FUNC:   return type_describe_func(&type->func); break;
    }
}
