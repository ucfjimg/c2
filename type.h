#pragma once

typedef enum {
    TT_INT,
    TT_FUNC,
} TypeTag;

typedef struct {
    int parms;                  // number of parameters
} TypeFunction;

typedef struct {
    TypeTag tag;

    union {
        TypeFunction func;      // TT_INT
    };
} Type;

extern Type *type_int(void);
extern Type *type_function(int parms);
extern void type_free(Type *type);
extern char *type_describe(Type *type);

