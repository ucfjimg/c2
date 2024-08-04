#include "symtab.h"

#include "fileline.h"
#include "safemem.h"
#include "type.h"

#include <stdio.h>

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
// Print the details of a function symbol.
//
static void stab_print_function(SymFunction *func)
{
    printf("    FUNCTION defined=%d global=%d\n", func->defined, func->global);
}

//
// Print the details of a static variable symbol.
//
static void stab_print_static_var(SymStaticVar *svar)
{
    printf("    STATIC ");
    switch (svar->siv) {
        case SIV_INIT:      printf("SIV_INIT "); break;
        case SIV_TENTATIVE: printf("SIV_TENTATIVE "); break;
        case SIV_NO_INIT:   printf("SIV_NO_INIT "); break;
    }

    char *loc = fileline_describe(&svar->loc);
    printf("init=%lu global=%d is_long=%d loc=%s\n", svar->initial, svar->global, svar->is_long, loc);
    safe_free(loc);
}

//
// Print the symbol table.
//
void stab_print(SymbolTable *stab)
{
    HashIterator iter;

    for (HashNode *curr = hashtab_first(stab->hashtab, &iter); curr; curr = hashtab_next(&iter)) {
        Symbol *sym = CONTAINER_OF(curr, Symbol, hash);

        printf("== %s", curr->key);
        if (sym->type) {
            char *type = type_describe(sym->type);
            printf(": %s", type);
            safe_free(type);
        }
        printf("\n");

        switch(sym->tag) {
            case ST_FUNCTION:   stab_print_function(&sym->func); break;
            case ST_STATIC_VAR: stab_print_static_var(&sym->stvar); break;
            case ST_LOCAL_VAR:  printf("LOCAL\n"); break;
        }
    }
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
void sym_update_static_var(Symbol *sym, Type *type, StaticInitialValue siv, unsigned long init, int flags, FileLine loc)
{
    sym->tag = ST_STATIC_VAR;

    if (sym->type != type) {
        type_free(sym->type);
        sym->type = type;
    }

    sym->stvar.initial = init;
    sym->stvar.siv = siv;
    sym->stvar.is_long = (flags & SUSV_LONG);
    sym->stvar.global = (flags & SUSV_GLOBAL);
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
