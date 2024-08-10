#pragma once

#include "list.h"

#include <stdbool.h>

typedef struct Type Type;

typedef enum {
    TT_INT,
    TT_LONG,
    TT_UINT,
    TT_ULONG,
    TT_DOUBLE,
    TT_FUNC,
    TT_POINTER,
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
    };
};

typedef struct {
    StorageClass sc;
    Type *type;
} TypeSpecifier;

extern Type *type_int(void);
extern Type *type_long(void);
extern Type *type_uint(void);
extern Type *type_ulong(void);
extern Type *type_double(void);
extern TypeFuncParam *type_func_param(Type *type);
extern void type_func_param_free(TypeFuncParam *param);
extern Type *type_pointer(Type *ref);
extern Type *type_function(Type *ret, List parms);
extern Type *type_clone(Type *type);
extern bool types_equal(Type *left, Type *right);
extern bool types_same_size(Type *left, Type *right);
extern bool type_unsigned(Type *type);
extern int type_rank(Type *type);
extern void type_free(Type *type);
extern char *type_describe(Type *type);


extern TypeSpecifier *typespec_alloc(StorageClass sc, Type *type);
extern void typespec_free(TypeSpecifier *typespec);
extern Type *typespec_take_type(TypeSpecifier *typespec);


