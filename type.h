#pragma once

#include "list.h"

#include <stdbool.h>

typedef struct Type Type;

typedef enum {
    TT_CHAR,
    TT_SCHAR,
    TT_UCHAR,
    TT_INT,
    TT_LONG,
    TT_UINT,
    TT_ULONG,
    TT_DOUBLE,
    TT_FUNC,
    TT_POINTER,
    TT_ARRAY,
} TypeTag;

typedef struct {
    ListNode list;
    Type *parmtype;
} TypeFuncParam;

typedef struct {
    Type *ret;                  // return value type
    List parms;                 // of <TypeFuncParam>
} TypeFunction;

typedef struct {
    Type *ref;                  // referenced type
} TypePointer;

typedef struct {
    Type *element;              // array of this
    size_t size;                // of this many elements
} TypeArray;

typedef enum {
    SC_NONE,
    SC_STATIC,
    SC_EXTERN,
} StorageClass;

struct Type {
    TypeTag tag;

    union {
        TypeFunction    func;   // TT_FUNC
        TypePointer     ptr;    // TT_POINTER
        TypeArray       array;  // TT_ARRAY
    };
};

typedef struct {
    StorageClass sc;
    Type *type;
} TypeSpecifier;

extern Type *type_char(void);
extern Type *type_schar(void);
extern Type *type_uchar(void);
extern bool type_is_char(Type *type);
extern Type *type_int(void);
extern Type *type_long(void);
extern Type *type_uint(void);
extern Type *type_ulong(void);
extern Type *type_double(void);
extern TypeFuncParam *type_func_param(Type *type);
extern void type_func_param_free(TypeFuncParam *param);
extern Type *type_pointer(Type *ref);
extern Type *type_array(Type *element, size_t size);
extern Type *type_function(Type *ret, List parms);
extern Type *type_clone(Type *type);
extern bool types_equal(Type *left, Type *right);
extern bool types_same_size(Type *left, Type *right);
extern bool type_unsigned(Type *type);
extern bool type_arithmetic(Type *type);
extern bool type_integral(Type *type);
extern Type *type_array_element(Type *type);
extern size_t type_array_size(Type *type);
extern size_t type_size(Type *type);
extern int type_rank(Type *type);
extern void type_free(Type *type);
extern char *type_describe(Type *type);


extern TypeSpecifier *typespec_alloc(StorageClass sc, Type *type);
extern void typespec_free(TypeSpecifier *typespec);
extern Type *typespec_take_type(TypeSpecifier *typespec);


