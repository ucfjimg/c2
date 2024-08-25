#pragma once

#include "list.h"
#include "hashtab.h"
#include "type.h"

typedef struct {
    ListNode list;

    char *membname;                 // member name
    Type *type;                     // member type
    unsigned long offset;           // offset from base of struct
} TypetabStructMember;

typedef struct {
    int align;                      // power-of-two struct alignment
    size_t size;                    // total size of struct
    List members;                   // of <TypetabStructMember>                   
} TypetabStruct;

typedef struct {
    HashNode hash;
    TypetabStruct struct_;          // Only track structures
} TypetabEnt;

typedef struct TypeTable {
    HashTable *hashtab;
} TypeTable;

extern TypeTable *typetab_alloc(void);
extern void typetab_free(TypeTable *tab);
extern TypetabEnt *typetab_lookup(TypeTable *tab, char *name);
extern void typetab_print(TypeTable *tab);

extern TypetabStructMember *ttab_struct_member(char *name, Type *type, unsigned long offset);
