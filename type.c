#include "type.h"

#include "ice.h"
#include "safemem.h"
#include "strbuilder.h"

#include <stdbool.h>

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
// Constructor for a long type.
//
Type *type_long(void)
{
    return type_alloc(TT_LONG);
}

//
// Constructor for a type parameter.
//
TypeFuncParam *type_func_param(Type *type)
{
    TypeFuncParam *tfp = safe_zalloc(sizeof(TypeFuncParam));
    tfp->parmtype = type;
    return tfp;
}

//
// Constructor for a function type.
//
Type *type_function(Type *ret, List parms)
{
    Type *type = type_alloc(TT_FUNC);
    type->func.ret = ret;
    type->func.parms = parms;
    return type;
}

//
// Clone a function parameter.
//
static TypeFuncParam *type_func_param_clone(TypeFuncParam *parm)
{
    TypeFuncParam *clone = safe_zalloc(sizeof(TypeFuncParam));
    clone->parmtype = type_clone(parm->parmtype);
    return clone;
}

//
// Clone a function type.
//
static Type *type_clone_func(TypeFunction *func)
{
    List new_parms;
    list_clear(&new_parms);

    for (ListNode *curr = func->parms.head; curr; curr = curr->next) {
        TypeFuncParam *param = CONTAINER_OF(curr, TypeFuncParam, list);
        param = type_func_param_clone(param);
        list_push_back(&new_parms, &param->list);
    }

    return type_function(type_clone(func->ret), new_parms);
}

//
// Clone a type.
//
Type *type_clone(Type *type)
{
    switch (type->tag) {
        case TT_INT:    return type_int();
        case TT_LONG:   return type_long();
        case TT_FUNC:   return type_clone_func(&type->func);
    }

    ICE_ASSERT(((void)"invalid type tag in type_clone", false));
    return NULL;
}

//
// Return true if two function types are equal.
//
bool types_func_equal(TypeFunction *left, TypeFunction *right)
{
    if (!types_equal(left->ret, right->ret)) {
        return false;
    }

    ListNode *leftcurr = left->parms.head;
    ListNode *rightcurr = right->parms.head;

    while (leftcurr && rightcurr) {
        TypeFuncParam *leftparm = CONTAINER_OF(leftcurr, TypeFuncParam, list);
        TypeFuncParam *rightparm = CONTAINER_OF(rightcurr, TypeFuncParam, list);

        if (!types_equal(leftparm->parmtype, rightparm->parmtype)) {
            return false;
        }

        leftcurr = leftcurr->next;
        rightcurr = rightcurr->next;
    }

    return leftcurr == NULL && rightcurr == NULL;
}

//
// Return true if the two types are identical.
//
bool types_equal(Type *left, Type *right)
{
    if (left == NULL || right == NULL) {
        return false;
    }

    if (left->tag != right->tag) {
        return false;
    }

    switch (left->tag) {
        case TT_INT:    return true;
        case TT_LONG:   return true;
        case TT_FUNC:   return types_func_equal(&left->func, &right->func);
    }

    ICE_ASSERT(((void)"invalid type tag in types_equal", false));
    return false;
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
    StrBuilder *desc = stb_alloc();

    if (func->ret == NULL) {
        stb_printf(desc, "void (");
    } else {
        char *ret = type_describe(func->ret);
        stb_printf(desc, "%s (", ret);
        safe_free(ret);
    }
    
    if (func->parms.head == NULL) {
        stb_printf(desc, "void");
    } else {
        for (ListNode *curr = func->parms.head; curr; curr = curr->next) {
            TypeFuncParam *param = CONTAINER_OF(curr, TypeFuncParam, list);

            char *ty = type_describe(param->parmtype);
            stb_printf(desc, "%s", ty);
            safe_free(ty);

            if (curr->next) {
                stb_printf(desc, ", ");
            }
        }
    }
    stb_printf(desc, ")");

    char *final = stb_take(desc);
    stb_free(desc);

    return final;
}

//
// Return an allocated string describing a type.
//
char *type_describe(Type *type)
{
    switch (type->tag) {
        case TT_INT:    return saprintf("int");
        case TT_LONG:   return saprintf("long");
        case TT_FUNC:   return type_describe_func(&type->func); break;
    }

    return saprintf("<invalid-type>");
}

//
// Construct a type specifier.
//
TypeSpecifier *typespec_alloc(StorageClass sc, Type *type)
{
    TypeSpecifier *ts = safe_zalloc(sizeof(TypeSpecifier));

    ts->sc = sc;
    ts->type = type;

    return ts;
}

//
// Free a type specifier.
//
void typespec_free(TypeSpecifier *typespec)
{
    if (typespec) {
        type_free(typespec->type);
        safe_free(typespec);
    }
}

//
// Take ownership of the type in a type specifier.
//
Type *typespec_take_type(TypeSpecifier *typespec)
{
    Type *type = typespec->type;
    typespec->type = NULL;
    return type;
}
