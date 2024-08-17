#include "list.h"

#include "safemem.h"

#include <string.h>

//
// Clear a list without deallocating
//
void list_clear(List *list)
{
    list->head = NULL;
    list->tail = NULL;
}

//
// Push a list node onto the end of a list.
//
void list_push_back(List *list, ListNode *node)
{
    if (node == NULL) {
        return;
    }

    node->next = NULL;

    if (list->head) {
        list->tail->next = node;
    } else {
        list->head = node;
    }
    
    list->tail = node;
}

//
// Push a list node onto the front of a list. 
//
void list_push_front(List *list, ListNode *node)
{
    if (node == NULL) {
        return;
    }

    if (list->head) {
        node->next = list->head;
    } else {
        list->tail = node;
    }

    list->head = node;
}

//
// Append a chain of nodes to an existing list.
//
void list_append(List *list, ListNode *head)
{
    if (head == NULL) {
        return;
    }

    ListNode *tail = head;

    while (tail->next) {
        tail = tail->next;
    }

    if (list->head) {
        list->tail->next = head;
    } else {
        list->head = head;
    }
    list->tail = tail;
}

//
// Return the number of elements in a list
//
size_t list_count(List *list)
{
    size_t count = 0;
    ListNode *node;

    for (node = list->head; node; node = node->next) {
        count++;
    }

    return count;
}

//
// Reverse a linked list in place.
// 
void list_reverse(List *list)
{
    ListNode *prev;
    ListNode *curr;
    ListNode *next;

    if (list->head == NULL) {
        return;
    }
    
    prev = NULL;
    curr = list->head;
    
    list->head = list->tail;
    list->tail = curr;

    while (curr) {
        next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }
}
