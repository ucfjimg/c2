#ifndef _HASHTAB_H_
#define _HASHTAB_H_

#include "list.h"

#define HASHTAB_SIZE    61

typedef struct
{
    ListNode list;
    unsigned hash;
    char *key;
} HashNode;

typedef HashNode *(*HashNodeAllocFn)(void);
typedef void (*HashNodeFreeFn)(HashNode *);

typedef struct
{
    List table[HASHTAB_SIZE];
    HashNodeAllocFn alloc_node;
    HashNodeFreeFn free_node;
} HashTable; 

typedef struct
{
    HashTable *tab;
    int bucket;
    HashNode *node;
} HashIterator;

extern HashTable *hashtab_alloc(HashNodeAllocFn alloc_fn, HashNodeFreeFn free_fn);
extern void hashtab_free(HashTable *tab);
extern HashNode *hashtab_lookup(HashTable *tab, char *key);
extern HashNode *hashtab_first(HashTable *tab, HashIterator *iter);
extern HashNode *hashtab_next(HashIterator *iter);

#endif

