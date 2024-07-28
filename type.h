#pragma once

typedef enum {
    TT_INT,
    TT_FUNC,
} TypeTag;

typedef struct {
    int parms;                  // number of parameters
} TypeFunction;

typedef enum {
    SC_NONE,
    SC_STATIC,
    SC_EXTERN,
} StorageClass;

typedef struct {
    TypeTag tag;

    union {
        TypeFunction func;      // TT_INT
    };
} Type;

typedef struct {
    StorageClass sc;
    Type *type;
} TypeSpecifier;

extern Type *type_int(void);
extern Type *type_function(int parms);
extern void type_free(Type *type);
extern char *type_describe(Type *type);

extern TypeSpecifier *typespec_alloc(StorageClass sc, Type *type);
extern void typespec_free(TypeSpecifier *typespec);

