
/*
 * mylist.c
 */

#include <stdio.h>
#include <stdlib.h>
#include "mylist.h"

struct Node *addLeft(struct List *list, void *data)
{
    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    if (node == NULL)
	return NULL;

    node->data = data;
    node->next = list->head;
    list->head = node;
    return node;
}

void traverseList(struct List *list, void (*f)(void *))
{
    struct Node *node = list->head;
    while (node) {
	f(node->data);
	node = node->next;
    }
}



void* accessL(struct List *list, int n)
{
    struct Node *node = list->head;
    int i = 0;
    while (i<n) {
	node = node->next;
    i+=1;
    }
    return node->data;
}


void flipSignDouble(void *data)
{
    *(double *)data = *(double *)data * -1;
}

int compareDouble(const void *data1, const void *data2)
{
    if (*(double *)data1 == *(double *)data2)
	return 0;
    else 
	return 1;
}

struct Node *findNode(struct List *list, const void *dataSought,
	int (*compar)(const void *, const void *))
{
    struct Node *node = list->head;
    while (node) {
	if (compar(dataSought, node->data) == 0)
	    return node;
	node = node->next;
    }
    return NULL;
}

void *removeRight(struct List *list)
{
    if (isEmptyList(list))
	return NULL;

    // find the next to last node
    struct Node *end = list->head;
    while (end->next && end->next->next)
	end = end->next;

    void *data = end->next->data;
    end->next = NULL;
    free(end);
    return data;
}

void *removeLeft(struct List *list)
{
    if (isEmptyList(list))
	return NULL;

    struct Node *oldHead = list->head;
    list->head = oldHead->next;
    void *data = oldHead->data;
    free(oldHead);
    return data;
}

void removeAllNodes(struct List *list)
{
    while (!isEmptyList(list))
	removeLeft(list);
}

struct Node *addAfter(struct List *list, 
	struct Node *prevNode, void *data)
{
    if (prevNode == NULL)
	return addLeft(list, data);

    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    if (node == NULL)
	return NULL;

    node->data = data;
    node->next = prevNode->next;
    prevNode->next = node;
    return node;
}

void reverseList(struct List *list)
{
    struct Node *prv = NULL;
    struct Node *cur = list->head;
    struct Node *nxt;

    while (cur) {
	nxt = cur->next;
	cur->next = prv;
	prv = cur;
	cur = nxt;
    }

    list->head = prv;
}

struct Node *addRight(struct List *list, void *data)
{
    // make the new node that will go to the end of list
    struct Node *node = (struct Node *)malloc(sizeof(struct Node));
    if (node == NULL)
	return NULL;
    node->data = data;
    node->next = NULL;

    // if the list is empty, this node is the head
    if (list->head == NULL) {
	list->head = node;
	return node;
    }

    // find the last node
    struct Node *end = list->head;
    while (end->next != NULL)
	end = end->next;

    // 'end' is the last node at this point
    end->next = node;
    return node;
}

/*
struct Node *initialize_int(int list[]){
    int length = (int)( sizeof(list) / sizeof(list[0]));

    struct Node *head = addLeft(NULL, &list[0]);
    struct Node *curr = head;

    for (int i = 1; i<length; i++){
        curr = addRight(curr, &list[i]);
    }
    return head;
}
*/

struct List *initialize(void* list, int length, int type){
    int *intList;
    char **charList;

	if (type) {
        intList = (int *) list;
    } else {
        charList = (char **) list;
	}

    struct List *lst = (struct List *) malloc(sizeof(struct List));

    struct Node *head;
    if (type) head = addLeft(lst, &intList[0]);
    else head = addLeft(lst, &charList[0]);

    lst->head = head;

    struct Node *curr = head;

    for (int i = 1; i < length; i++){
        struct Node *node = (struct Node *) malloc(sizeof(struct Node));
        if (node == NULL)
        return NULL;

        curr->next = node;

        if (type) node->data = &intList[i];
        else node->data = &charList[i];
        
        node->next = NULL;

        curr = node;
    }

    return lst;
}

