#pragma once

#include "hashtab.h"
#include "type.h"

#include <stdbool.h>

typedef struct {
    HashNode hash;
    Type *type;
    bool defined;
} Symbol;

typedef struct {
    HashTable *hashtab;
} SymbolTable;

extern SymbolTable *stab_alloc(void);
extern void stab_free(SymbolTable *stab);
extern Symbol *stab_lookup(SymbolTable *stab, char *name);

