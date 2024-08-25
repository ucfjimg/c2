#include "typetab.h"

#include "safemem.h"

#include <stdio.h>

//
// Allocate a new type table node.
//
static HashNode *typetab_alloc_entry(void)
{
    TypetabEnt *ent = safe_zalloc(sizeof(TypetabEnt));
    return &ent->hash;
}

//
// Free a type table node.
//
static void typetab_free_entry(HashNode *hash)
{
    TypetabEnt *ent = CONTAINER_OF(hash, TypetabEnt, hash);

    ListNode *next = NULL;
    for (ListNode *curr = ent->struct_.members.head; curr; curr = next) {
        next = curr->next;

        TypetabStructMember *memb = CONTAINER_OF(curr, TypetabStructMember, list);
        type_free(memb->type);
        safe_free(memb);
    }
}

//
// Allocate a type table instance.
//
TypeTable *typetab_alloc(void)
{
    TypeTable *typetab = safe_zalloc(sizeof(TypeTable));
    typetab->hashtab = hashtab_alloc(typetab_alloc_entry, typetab_free_entry);
    return typetab;
}

//
// Free the type table.
//
void typetab_free(TypeTable *tab)
{
    if (tab) {
        hashtab_free(tab->hashtab);
        safe_free(tab);
    }
}

//
// Look up an entry in the type table.
//
TypetabEnt *typetab_lookup(TypeTable *tab, char *name)
{
    HashNode *node = hashtab_lookup(tab->hashtab, name);
    TypetabEnt *ent = CONTAINER_OF(node, TypetabEnt, hash);
    return ent;
}

//
// Print the type table.
// 
void typetab_print(TypeTable *tab)
{
    HashIterator iter;

    for (HashNode *curr = hashtab_first(tab->hashtab, &iter); curr; curr = hashtab_next(&iter)) {
        TypetabEnt *ent = CONTAINER_OF(curr, TypetabEnt, hash);
                
        printf("== %s", curr->key);
        printf("struct {    // align=%d size=%zd\n", ent->struct_.align, ent->struct_.size);

        for (ListNode *curr = ent->struct_.members.head; curr; curr = curr->next) {
            TypetabStructMember *memb = CONTAINER_OF(curr, TypetabStructMember, list);
            char *desc = type_describe(memb->type);

            printf("    %s %s;  // offset=0x%zx\n", desc, memb->membname, memb->offset);
        }

        printf("};\n");
    }
}

//
// Constructor for a type table struct member.
//
TypetabStructMember *ttab_struct_member(char *name, Type *type, unsigned long offset)
{
    TypetabStructMember *tsm = safe_zalloc(sizeof(TypetabStructMember));

    tsm->membname = safe_strdup(name);
    tsm->type = type;
    tsm->offset = offset;

    return tsm;
}
