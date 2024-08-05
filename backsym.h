#pragma once

#include "asm-ast.h"
#include "hashtab.h"

#include <stdbool.h>

typedef enum {
    BST_OBJECT,
    BST_FUNCTION
} BackEndSymbolTag;

typedef struct {
    AsmType *type;
    bool is_static;
} BackEndObject;

typedef struct {
    bool is_defined;
} BackEndFunction;

typedef struct {
    HashNode hash;
    BackEndSymbolTag tag;

    union {
        BackEndObject   object;
        BackEndFunction func;    
    };
} BackEndSymbol;

typedef struct {
    HashTable *hashtab;
} BackEndSymbolTable;

extern BackEndSymbolTable *bstab_alloc(void);
extern void bstab_free(BackEndSymbolTable *bstab);
extern BackEndSymbol *bstab_lookup(BackEndSymbolTable *bstab, char *name);
extern void bstab_print(BackEndSymbolTable *bstab);

