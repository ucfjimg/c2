#pragma once

#include <stddef.h>

typedef struct ListNode ListNode;

struct ListNode
{
    ListNode *next;
};

typedef struct
{
    ListNode *head;
    ListNode *tail;
} List;

extern void list_clear(List *list);
extern void list_push_back(List *list, ListNode *node);
extern void list_push_front(List *list, ListNode *node);
extern void list_append(List *list, ListNode *head);
extern void list_reverse(List *list);
extern int list_count(List *list);

//
// CONTAINER_OF(listnode, container_type, listnode_member)
//
#define CONTAINER_OF(listnode, nodetype, member) \
    ((nodetype*)(((char*)(listnode)) - offsetof(nodetype, member)))

