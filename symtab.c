#include "symtab.h"

#include "fileline.h"
#include "ice.h"
#include "safemem.h"
#include "strutil.h"
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
// Print a constant.
//
static void stab_print_const(Const *cn)
{
    switch (cn->tag) {
        case CON_INTEGRAL:
            switch (cn->intval.size) {
                case CIS_CHAR:
                    if (cn->intval.sign == CIS_SIGNED) {
                        printf("%d", (signed char)cn->intval.value);
                    } else {
                        printf("%u", (unsigned char)cn->intval.value);
                    }
                    break;
                case CIS_INT:
                    if (cn->intval.sign == CIS_SIGNED) {
                        printf("%d", (signed int)cn->intval.value);
                    } else {
                        printf("%u", (unsigned int)cn->intval.value);
                    }
                    break;
                case CIS_LONG:
                    if (cn->intval.sign == CIS_SIGNED) {
                        printf("%ld", (signed long)cn->intval.value);
                    } else {
                        printf("%lu", (unsigned long)cn->intval.value);
                    }
                    break;
            }
            break;

        case CON_FLOAT:
            printf("%g", cn->floatval);
            break;
    }
}

//
// Print a string constant.
//
static void stab_print_string(StaticInitString *str)
{
    char *desc = str_escape(str->data, str->length);
    printf("\"%s\"", desc);
    safe_free(desc);
}

//
// Print one static intializer.
//
static void stab_print_static_init(StaticInitializer *si)
{
    switch (si->tag) {
        case SI_CONST:  printf("const "); stab_print_const(&si->cval); break;
        case SI_ZERO:   printf("zero(%zd)", si->bytes); break;
        case SI_STRING: printf("string "); stab_print_string(&si->string); break;
        case SI_POINTER:printf("pointer %s", si->ptr_name); break;
    }

    printf("\n");
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


    if (svar->initial.head) {
        printf("= {\n");

        for (ListNode *curr = svar->initial.head; curr; curr = curr->next) {
            StaticInitializer *si = CONTAINER_OF(curr, StaticInitializer, list);
            stab_print_static_init(si);
        }

        printf("}");
    }
    printf("\n");
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
            case ST_CONSTANT:   printf("ST-CONST "); stab_print_static_init(sym->stconst); break;
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
void sym_update_static_var(Symbol *sym, Type *type, StaticInitialValue siv, List init, bool global, FileLine loc)
{
    sym->tag = ST_STATIC_VAR;

    if (sym->type != type) {
        type_free(sym->type);
        sym->type = type;
    }

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

//
// Construct a constant static initializer.
//
StaticInitializer *sinit_make_const(Const cn)
{
    StaticInitializer *si = safe_zalloc(sizeof(StaticInitializer));

    si->tag = SI_CONST;
    si->cval = cn;

    return si;
}

//
// Construct a zero-fill static initializer.
//
StaticInitializer *sinit_make_zero(size_t bytes)
{
    StaticInitializer *si = safe_zalloc(sizeof(StaticInitializer));

    si->tag = SI_ZERO;
    si->bytes = bytes;

    return si;
}

//
// Construct a string constant static initializer.
//
StaticInitializer *sinit_make_string(char *data, size_t length, bool nul_terminated)
{
    StaticInitializer *si = safe_zalloc(sizeof(StaticInitializer));

    si->tag = SI_STRING;
    si->string.data = safe_malloc(length);
    memcpy(si->string.data, data, length);
    si->string.length = length;
    si->string.nul_terminated = nul_terminated;

    return si;
}

//
// Construct a pointer initializer, which points to another named static object.
//
StaticInitializer *sinit_make_pointer(char *name)
{
    StaticInitializer *si = safe_zalloc(sizeof(StaticInitializer));

    si->tag = SI_POINTER;
    si->ptr_name = safe_strdup(name);

    return si;
}

//
// Clone a static initializer.
//
StaticInitializer *sinit_clone(StaticInitializer *si)
{
    switch (si->tag) {
        case SI_CONST:      return sinit_make_const(si->cval);
        case SI_ZERO:       return sinit_make_zero(si->bytes);
        case SI_STRING:     return sinit_make_string(si->string.data, si->string.length, si->string.nul_terminated);
        case SI_POINTER:    return sinit_make_pointer(si->ptr_name);
    }

    ICE_ASSERT(((void)"invalid tag in sinit_clone", false));
    return NULL;
}
