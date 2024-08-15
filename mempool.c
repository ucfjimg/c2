#include "mempool.h"

#include "ice.h"
#include "safemem.h"

#include <stddef.h>

#define PAGE_SIZE 4096

static void mp_refill(MemPool *mp);

//
// Create a pool to allocate blocks of size `block_size`.
//
MemPool* mp_create(int block_size)
{
    ICE_ASSERT(block_size > 0);
    MemPool *mp = safe_zalloc(sizeof(MemPool));

    mp->block_size = block_size;
    mp->page_size = PAGE_SIZE;

    int block_space = PAGE_SIZE - (sizeof(MemPoolPage) - offsetof(MemPoolPage, blocks));
    ICE_ASSERT(block_space > 0);

    mp->blocks_per_page = block_space / block_size;
    ICE_ASSERT(mp->blocks_per_page > 0);

    list_clear(&mp->pages);

    return mp;
}

//
// Destroy the pool, including all allocations.
//
void mp_destroy(MemPool *mp)
{
    mp_free_all(mp);
    safe_free(mp);
}

//
// Allocate a block of the fixed size.
//
void *mp_alloc(MemPool *mp)
{
    if (mp->pages.head == NULL) {
        mp_refill(mp);
    }

    MemPoolPage *page = CONTAINER_OF(mp->pages.tail, MemPoolPage, list);

    if (page->in_use == mp->blocks_per_page) {
        mp_refill(mp);
        page = CONTAINER_OF(mp->pages.tail, MemPoolPage, list);
    }

    void *blk = &page->blocks[page->in_use * mp->block_size];
    page->in_use++;

    return blk;
}

//
// Free all blocks in the pool, but not the pool itself.
//
void mp_free_all(MemPool *mp)
{
    ListNode *next = NULL;
    for (ListNode *curr = mp->pages.head; curr; curr = next) {
        next = curr->next;

        MemPoolPage *page = CONTAINER_OF(curr, MemPoolPage, list);
        safe_free(page);
    }

    list_clear(&mp->pages);
}

//
// Add a new empty page at the end of the list.
//
void mp_refill(MemPool *mp)
{
    MemPoolPage *page = safe_zalloc(mp->page_size);
    page->in_use = 0;
    list_push_back(&mp->pages, &page->list);
}
