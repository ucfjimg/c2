#pragma once

#include "hashtab.h"
#include "fileline.h"
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
    bool is_long;                   // initializer is of type long
    unsigned long initial;          // if SIV_INIT, the initial value
    bool global;                    // variable is globally visible
    FileLine loc;                   // where declared
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
extern void stab_print(SymbolTable *stab);

extern void sym_update_func(Symbol *sym, Type *type, bool defined, bool global);

#define SUSV_GLOBAL 0x0001
#define SUSV_LONG   0x0002

extern void sym_update_static_var(Symbol *sym, Type *type, StaticInitialValue siv, unsigned long init, int flags, FileLine loc);
extern void sym_update_local(Symbol *sym, Type *type);



