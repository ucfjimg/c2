#pragma once

#include "hashtab.h"
#include "type.h"

#include <stdbool.h>

typedef enum {
    ST_FUNCTION,
    ST_STATIC_VAR,
    ST_LOCAL_VAR,
} SymbolTag;

typedef struct {
    bool defined;                   // function has a body
    bool global;                    // function is globally visible
} SymFunction;

typedef enum {
    SIV_TENTATIVE,                  // no initializer yet
    SIV_INIT,                       // initializer
    SIV_NO_INIT,                    // no initializer
} StaticInitialValue;

typedef struct {
    StaticInitialValue siv;         // initializer type
    unsigned long initial;          // if SIV_INIT, the initial value
    bool explicit_init;             // if explicit initial value given
    bool global;                    // variable is globally visible
} SymStaticVar;

typedef struct {
    HashNode hash;
    Type *type;                     // declaration type
    SymbolTag tag;                  // symbol type (func, var, static var)

    union {
        SymFunction     func;       // function attributes
        SymStaticVar    stvar;      // static variable attributes
    };
} Symbol;

typedef struct {
    HashTable *hashtab;
} SymbolTable;

extern SymbolTable *stab_alloc(void);
extern void stab_free(SymbolTable *stab);
extern Symbol *stab_lookup(SymbolTable *stab, char *name);

extern void sym_update_func(Symbol *sym, Type *type, bool defined, bool global);
extern void sym_update_static_var(Symbol *sym, Type *type, StaticInitialValue siv, unsigned long init, bool explicit_init, bool global);
extern void sym_update_local(Symbol *sym, Type *type);



