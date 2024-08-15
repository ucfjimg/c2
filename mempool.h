#pragma once

#include "list.h"

//
// a pool allocates objects of one size, and can only free them all at once.
//

typedef struct {
    ListNode list;              // keep linked list of pages
    int in_use;                 // how many blocks are in use in this page
    char blocks[0];             // where blocks start           
} MemPoolPage;

typedef struct {
    int block_size;             // size of block to return
    int page_size;              // size of page to allocate
    int blocks_per_page;        // how many blocks on each page
    List pages;                 // list of pages (last is partially full)
} MemPool;

extern MemPool* mp_create(int block_size);
extern void mp_destroy(MemPool *mp);
extern void *mp_alloc(MemPool *mp);
extern void mp_free_all(MemPool *mp);
