#include "type.h"

#include "ice.h"
#include "safemem.h"
#include "strbuilder.h"
#include "target.h"

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
// Constructor for a char type.
//
Type *type_char(void)
{
    return type_alloc(TT_CHAR);
}

//
// Constructor for a signed char type.
//
Type *type_schar(void)
{
    return type_alloc(TT_SCHAR);
}

//
// Constructor for an unsigned char type.
//
Type *type_uchar(void)
{
    return type_alloc(TT_UCHAR);
}

//
// Return true if the given type is any character type.
//
bool type_is_char(Type *type)
{
    return
        type->tag == TT_CHAR ||
        type->tag == TT_SCHAR ||
        type->tag == TT_UCHAR;
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
// Constructor for an unsigned integer type.
//
Type *type_uint(void)
{
    return type_alloc(TT_UINT);
}

//
// Constructor for an unsigned long type.
//
Type *type_ulong(void)
{
    return type_alloc(TT_ULONG);
}

//
// Constructor for a double type.
//
Type *type_double(void)
{
    return type_alloc(TT_DOUBLE);
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
// Free a type parameter.
//
void type_func_param_free(TypeFuncParam *param)
{
    type_free(param->parmtype);
    safe_free(param);
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
// Constructor for a pointer.
//
Type *type_pointer(Type *ref)
{
    Type *type = type_alloc(TT_POINTER);
    type->ptr.ref = ref;
    return type;
}

//
// Constructor for an array.
//
Type *type_array(Type *element, size_t size)
{
    Type *type = type_alloc(TT_ARRAY);
    type->array.element = element;
    type->array.size = size;
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
// Clone a pointer type.
//
static Type *type_clone_pointer(TypePointer *ptr)
{
    return type_pointer(type_clone(ptr->ref));
}

//
// Clone an array type.
//
static Type *type_clone_array(TypeArray *array)
{
    return type_array(type_clone(array->element), array->size);
}

//
// Clone a type.
//
Type *type_clone(Type *type)
{
    switch (type->tag) {
        case TT_CHAR:   return type_char();
        case TT_SCHAR:  return type_schar();
        case TT_UCHAR:  return type_uchar();
        case TT_INT:    return type_int();
        case TT_LONG:   return type_long();
        case TT_UINT:   return type_uint();
        case TT_ULONG:  return type_ulong();
        case TT_DOUBLE: return type_double();
        case TT_FUNC:   return type_clone_func(&type->func);
        case TT_POINTER:return type_clone_pointer(&type->ptr);
        case TT_ARRAY:  return type_clone_array(&type->array);
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
// Return true if the two array types are identical.
//
bool types_array_equal(TypeArray *left, TypeArray *right)
{
    if (!types_equal(left->element, right->element)) {
        return false;
    }

    return left->size == right->size;
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
        case TT_CHAR:   return true;
        case TT_UCHAR:  return true;
        case TT_SCHAR:  return true;
        case TT_INT:    return true;
        case TT_LONG:   return true;
        case TT_UINT:   return true;
        case TT_ULONG:  return true;
        case TT_DOUBLE: return true;
        case TT_FUNC:   return types_func_equal(&left->func, &right->func);
        case TT_POINTER:return types_equal(left->ptr.ref, right->ptr.ref);
        case TT_ARRAY:  return types_array_equal(&left->array, &right->array);
    }

    ICE_ASSERT(((void)"invalid type tag in types_equal", false));
    return false;
}

//
// Return true if two types are the same size, regardless of sign.
//
bool types_same_size(Type *left, Type *right)
{
    //
    // $TARGET we are assumuing that sizeof(LONG) == sizeof(pointer)
    //
    bool left_long = left->tag == TT_LONG || left->tag == TT_ULONG || left->tag == TT_POINTER;
    bool right_long = right->tag == TT_LONG || right->tag == TT_ULONG || right->tag == TT_POINTER;

    return left_long == right_long;
}

//
// Return true if the given type is an unsigned integral type.
//
bool type_unsigned(Type *type)
{
    switch (type->tag) {
#ifndef TARGET_CHAR_SIGNED
        case TT_CHAR:
#endif
        case TT_UINT:
        case TT_UCHAR:
        case TT_ULONG:  return true;

#ifdef TARGET_CHAR_SIGNED
        case TT_CHAR:
#endif
        case TT_SCHAR:
        case TT_INT:
        case TT_LONG:
        case TT_DOUBLE:
        case TT_FUNC:   
        case TT_POINTER:
        case TT_ARRAY:  return false;
    }

    return false;
}

//
// Return true if the given type is an arithmetic type.
//
bool type_arithmetic(Type *type)
{
    switch (type->tag) {
        case TT_CHAR:
        case TT_SCHAR:
        case TT_UCHAR:     
        case TT_UINT:
        case TT_ULONG: 
        case TT_INT:
        case TT_LONG:
        case TT_DOUBLE: return true;
        case TT_FUNC:   
        case TT_POINTER:
        case TT_ARRAY:  return false;
    }

    return false;
}

//
// Return true if the given type is an integral type.
//
bool type_integral(Type *type)
{
    switch (type->tag) {
        case TT_CHAR:
        case TT_SCHAR:
        case TT_UCHAR:
        case TT_UINT:
        case TT_ULONG: 
        case TT_INT:
        case TT_LONG:   return true;
        case TT_DOUBLE:
        case TT_FUNC:   
        case TT_POINTER:
        case TT_ARRAY:  return false;
    }

    return false;
}

//
// Return the base element type of a (possibly nested) array.
//
Type *type_array_element(Type *type)
{
    if (type->tag == TT_ARRAY) {
        return type_array_element(type->array.element);
    }
    return type;
}

//
// Return the number of the elements in the (potentially nested) array.
// If the type is not an array, returns 1.
//
size_t type_array_size(Type *type)
{
    if (type->tag == TT_ARRAY) {
        return type->array.size * type_array_size(type->array.element);
    }

    return 1;
}

//
// Return the total size of the type, in bytes.
//
size_t type_size(Type *type)
{
    switch (type->tag) {
        case TT_CHAR:
        case TT_SCHAR:
        case TT_UCHAR:      return 1;

        case TT_INT:        return TARGET_INT_SIZE;
        case TT_LONG:       return TARGET_LONG_SIZE; 
        case TT_UINT:       return TARGET_INT_SIZE;
        case TT_ULONG:      return TARGET_LONG_SIZE;
        case TT_DOUBLE:     return TARGET_DOUBLE_SIZE;
        case TT_POINTER:    return TARGET_POINTER_SIZE;

        case TT_ARRAY:      return type->array.size * type_size(type->array.element);

        case TT_FUNC:       ICE_ASSERT(((void)"type_size asked for size of function", false));
    }

    ICE_ASSERT(((void)"invalid type tag in type_size", false));    
    return 0;
}

//
// Return the rank of a base type.
//
int type_rank(Type *type)
{
    switch (type->tag) {
        case TT_DOUBLE: return 5;
        case TT_ULONG:  
        case TT_POINTER:return 4;
        case TT_LONG:   return 3;
        case TT_UINT:   return 2;
        case TT_INT:    return 1;
        case TT_CHAR:   return 1;
        case TT_SCHAR:  return 1;
        case TT_UCHAR:  return 1;
        case TT_FUNC:   
        case TT_ARRAY:  ICE_ASSERT(((void)"non-base type is invalid in type_rank", false));
    }

    return 0;
}

//
// Free a function type.
//
static void type_free_function(TypeFunction *func)
{
    type_free(func->ret);

    ListNode *next = NULL;
    for (ListNode *curr = func->parms.head; curr; curr = next) {
        next = curr->next;

        TypeFuncParam *tfp = CONTAINER_OF(curr, TypeFuncParam, list);
        type_free(tfp->parmtype);
        safe_free(tfp);
    }
}

//
// Free an array type.
//
static void type_free_array(TypeArray *array)
{
    type_free(array->element);
}

//
// Free a type.
//
void type_free(Type *type)
{
    if (type) {
        switch (type->tag) {
            case TT_CHAR:
            case TT_SCHAR:
            case TT_UCHAR:
            case TT_INT:
            case TT_LONG:
            case TT_UINT:
            case TT_ULONG:
            case TT_DOUBLE:     break;
            case TT_FUNC:       type_free_function(&type->func); break;
            case TT_POINTER:    break;
            case TT_ARRAY:      type_free_array(&type->array); break;       
        }

        safe_free(type);
    }
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
        stb_printf(desc, "(%s)(", ret);
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
// Return an allocated string describing a function type.
//
static char *type_describe_ptr(TypePointer *ptr)
{
    char *ref = type_describe(ptr->ref);
    char *desc = saprintf("(%s)*", ref);
    safe_free(ref);
    return desc;
}

//
// Return an allocated string describing an array type.
//
static char *type_describe_array(TypeArray *array)
{
    char *ele = type_describe(array->element);
    char *desc = saprintf("(%s)[%zd]", ele, array->size);
    safe_free(ele);
    return desc;
}

//
// Return an allocated string describing a type.
//
char *type_describe(Type *type)
{
    switch (type->tag) {
        case TT_CHAR:   return saprintf("char");
        case TT_SCHAR:  return saprintf("signed char");
        case TT_UCHAR:  return saprintf("unsigned char");
        case TT_INT:    return saprintf("int");
        case TT_LONG:   return saprintf("long");
        case TT_UINT:   return saprintf("unsigned int");
        case TT_ULONG:  return saprintf("unsigned long");
        case TT_DOUBLE: return saprintf("double");
        case TT_FUNC:   return type_describe_func(&type->func); break;
        case TT_POINTER:return type_describe_ptr(&type->ptr); break;
        case TT_ARRAY:  return type_describe_array(&type->array); break;
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
