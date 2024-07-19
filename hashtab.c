#include "hashtab.h"

#include "safemem.h"

#include <string.h>

#define LIST_NODE_TO_HASH(ln) \
    CONTAINER_OF(ln, struct hashnode, next)

static unsigned hash_str(char *str);

//
// Alloc and initialize a hash table. the caller is responsible for
// setting up the alloc_node and free_node members.
//
HashTable *hashtab_alloc(HashNodeAllocFn alloc_fn, HashNodeFreeFn free_fn)
{
    HashTable *tab = safe_zalloc(sizeof(HashTable));

    for (int i = 0; i < HASHTAB_SIZE; i++) {
        list_clear(&tab->table[i]);
    }

    tab->alloc_node = alloc_fn;
    tab->free_node = free_fn;

    return tab;
}

// 
// Free all members in the hash table, and then the
// hash table. 
//
void hashtab_free(HashTable *tab)
{
    if (tab) {
        for (int i = 0; i < HASHTAB_SIZE; i++) {
            for (ListNode *curr = tab->table[i].head; curr; ) {
                ListNode *next = curr->next;
                HashNode *node = CONTAINER_OF(curr, HashNode, list);
                
                safe_free(node->key);
                tab->free_node(node);

                curr = next;
            }
        }
    
        safe_free(tab);
    }
}

//
// Look up an entry in the hash table. if an entry with the
// passed key does not exist, a new entry will be created
// and returned.
//
// The new entry will be allocated with the caller's
// alloc_node() function. it is up to that function to
// initialize the new entry in a way such that the entry
// can be determined to be fresh.
//
HashNode *hashtab_lookup(HashTable *tab, char *key)
{
    unsigned h = hash_str(key);
    int slot = h % HASHTAB_SIZE;
    
    for (ListNode *curr = tab->table[slot].head; curr; curr = curr->next) {
        HashNode *node = CONTAINER_OF(curr, HashNode, list);

        if (node->hash == h && strcmp(node->key, key) == 0) {
            return node;
        }
    }
    HashNode *node = tab->alloc_node();
    node->hash = h;
    node->key = safe_strdup(key);
    
    list_push_front(&tab->table[slot], &node->list);

    return node;
}

//
// Start iterating a hash table at the first node. the nodes are
// returned in no particular order.
//
// To retrieve subsequent nodes, call hashtab_next() with the initialized
// iter struct.
//
HashNode *hashtab_first(HashTable *tab, HashIterator *iter)
{
    int bucket;

    iter->tab = tab;
    iter->node = NULL;

    for (bucket = 0; bucket < HASHTAB_SIZE; bucket++) {
        if (tab->table[bucket].head) {
            iter->node = CONTAINER_OF(tab->table[bucket].head, HashNode, list);
            break;
        }
    }

    iter->bucket = bucket;

    return iter->node; 
}

//
// Return the next node in the iteration sequence of a hash table. the nodes are
// returned in no particular order.
//
// The results of the iteration if the table is modified after the call to hashtab_first(),
// or between calls to hashtab_next(), is undefined.
//
HashNode *hashtab_next(HashIterator *iter)
{
    int bucket;

    if (iter->bucket == HASHTAB_SIZE) {
        return NULL;
    }

    if (iter->node->list.next) {
        iter->node = CONTAINER_OF(iter->node->list.next, HashNode, list);
    } else {
        iter->node = NULL;
    }

    if (iter->node == NULL) {
        //
        // Finished list from previous bucket, move to next.
        //
        for (bucket = iter->bucket + 1; bucket < HASHTAB_SIZE; bucket++) {
            if (iter->tab->table[bucket].head) {
                iter->node = CONTAINER_OF(iter->tab->table[bucket].head, HashNode, list);
                break;
            }
        }
        
        iter->bucket = bucket;
    }

    return iter->node;
}

//
// Hash a string. this is the well-known djb2 function.
//
unsigned hash_str(char *str)
{
    unsigned h = 5381;
    int c;

    while ((c = *str++) != '\0') {
        h = h * 33 + c;
    }

    return h;
}
