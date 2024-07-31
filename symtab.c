#include "symtab.h"

#include "fileline.h"
#include "safemem.h"

//
// Allocate a new symbol table node.
//
static HashNode *stab_alloc_symbol(void)
{
    Symbol *sym = safe_zalloc(sizeof(Symbol));
    return &sym->hash;
}

//
// Free a symbol table node.
//
static void stab_free_symbol(HashNode *hash)
{
    Symbol *sym = CONTAINER_OF(hash, Symbol, hash);

    type_free(sym->type);
    safe_free(sym);
}

//
// Allocate a new symbol table.
//
SymbolTable *stab_alloc(void)
{
    SymbolTable *stab = safe_zalloc(sizeof(SymbolTable));
    stab->hashtab = hashtab_alloc(stab_alloc_symbol, stab_free_symbol);
    return stab;
}

//
// Free a symbol table.
//
void stab_free(SymbolTable *stab)
{
    if (stab) {
        hashtab_free(stab->hashtab);
        safe_free(stab);
    }
}

//
// Look up an entry in the symbol table.
//
Symbol *stab_lookup(SymbolTable *stab, char *name)
{
    HashNode *node = hashtab_lookup(stab->hashtab, name);
    Symbol *sym = CONTAINER_OF(node, Symbol, hash);
    return sym;
}

//
// Update the given symbol to be a function.
//
void sym_update_func(Symbol *sym, Type *type, bool defined, bool global)
{
    sym->tag = ST_FUNCTION;
   
    if (sym->type != type) {
        type_free(sym->type);
        sym->type = type;
    }

    sym->func.defined = defined;
    sym->func.global = global;
}

//
// Update the given symbol to be a static variable.
//
void sym_update_static_var(Symbol *sym, Type *type, StaticInitialValue siv, unsigned long init, bool explicit_init, bool global, FileLine loc)
{
    sym->tag = ST_STATIC_VAR;

    if (sym->type != type) {
        type_free(sym->type);
        sym->type = type;
    }

    sym->stvar.explicit_init = explicit_init;
    sym->stvar.initial = init;
    sym->stvar.siv = siv;
    sym->stvar.global = global;
    sym->stvar.loc = loc;
}

//
// Update the given symbol to be a local variable.
//
extern void sym_update_local(Symbol *sym, Type *type)
{
    sym->tag = ST_LOCAL_VAR;
    
    if (sym->type != type) {
        type_free(sym->type);
        sym->type = type;
    }
}
