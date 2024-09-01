#include "backsym.h"

#include "safemem.h"

#include <stdio.h>

//
// Allocate a new symbol table node.
//
static HashNode *bstab_alloc_symbol(void)
{
    BackEndSymbol *sym = safe_zalloc(sizeof(BackEndSymbol));
    return &sym->hash;
}

//
// Free a symbol object.
//
static void bstab_free_object(BackEndObject *obj)
{
    asmtype_free(obj->type);
    obj->type = NULL;
}

//
// Free the interior fields of a symbol.
//
static void bstab_free_symbol(BackEndSymbol *sym)
{
    switch (sym->tag) {
        case BST_OBJECT:        bstab_free_object(&sym->object);
        case BST_FUNCTION:      break;
    }
}

//
// Free a symbol table node.
//
static void bstab_free_hashnode(HashNode *hash)
{
    BackEndSymbol *sym = CONTAINER_OF(hash, BackEndSymbol, hash);

    bstab_free_symbol(sym);
    safe_free(sym);
}

//
// Create a back end symbol table.
//
BackEndSymbolTable *bstab_alloc(void)
{
    BackEndSymbolTable *stab = safe_zalloc(sizeof(BackEndSymbolTable));
    stab->hashtab = hashtab_alloc(bstab_alloc_symbol, bstab_free_hashnode);
    return stab;
}

//
// Free a back end symbol table.
//
void bstab_free(BackEndSymbolTable *bstab)
{
    if (bstab) {
        hashtab_free(bstab->hashtab);
        safe_free(bstab);
    }
}

//
// Lookup a symbol in the back end symbol table.
//
BackEndSymbol *bstab_lookup(BackEndSymbolTable *bstab, char *name)
{
    HashNode *node = hashtab_lookup(bstab->hashtab, name);
    BackEndSymbol *sym = CONTAINER_OF(node, BackEndSymbol, hash);
    return sym;
}

//
// Print a back end object.
//
static void bstab_object_print(BackEndObject *obj)
{
    char *desc = asmtype_describe(obj->type);
    printf("  static %s type %s\n", 
        obj->is_static ? "yes" : "no",
        desc);
    safe_free(desc);
}

//
// Print a back end function.
//
static void bstab_function_print(BackEndFunction *obj)
{
    printf("  defined %s return_on_stack %s\n",
        obj->is_defined ? "yes" : "no",
        obj->return_on_stack ? "yes" : "no");
}

//
// Print the back end symbol stable.
//
void bstab_print(BackEndSymbolTable *bstab)
{
    HashIterator iter;

    for (HashNode *curr = hashtab_first(bstab->hashtab, &iter); curr; curr = hashtab_next(&iter)) {
        BackEndSymbol *sym = CONTAINER_OF(curr, BackEndSymbol, hash);

        printf("== %s\n", curr->key);

        switch (sym->tag) {
            case BST_OBJECT:    bstab_object_print(&sym->object); break;
            case BST_FUNCTION:  bstab_function_print(&sym->func); break;
        }
    }
}
