#include "symtab.h"

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

