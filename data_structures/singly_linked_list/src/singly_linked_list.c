#include <stdio.h>
#include <stdlib.h>

#include "singly_linked_list.h"

// A singly-linked node that composes an ::SLList_t.
struct SLNode {
	void *data; // The contained data.
	struct SLNode *next; // The next node in the sequence.
};

/*
 * @brief Allocate an ::SLNode_t.
 *
 * @param data To be stored in the node's ::SLNode_t::data.
 *
 * @return The new node.
*/
static SLNode_t *createSLNode(void *data);

/*
 * @brief Deallocate an ::SLNode_t
 *
 * @param node A node.
 *
 * @return The deallocated node's ::SLNode_t::data.
*/
static void *freeSLNode(SLNode_t *node);

SLList_t *createSLList(void (*freeData)(void *data)){
	SLList_t *list = malloc(sizeof(SLList_t));
	list->head = list->tail = NULL;
	list->len = 0;
	list->freeData = freeData;
	return list;
}

void freeSLList(SLList_t *list){
	SLNode_t *currNode = list->head;
	while(currNode != NULL){
		SLNode_t *nextNode = currNode->next;
		list->freeData(freeSLNode(currNode));
		currNode = nextNode;
	}
	free(list);
}

void insertSLListHead(SLList_t *list, void *data){
	list->len++;
	SLNode_t *newHead = createSLNode(data);
	newHead->next = list->head;
	list->head = newHead;
}

void *removeSLListHead(SLList_t *list){
	if(0 < list->len){
		list->len--;
		SLNode_t *head = list->head;
		list->head = list->head->next;
		return freeSLNode(head);
	}
	else
		return NULL;
}

void insertAfterSLNode(SLList_t *list, SLNode_t *node, void *data){
	list->len++;
	SLNode_t *newNode = createSLNode(data);
	newNode->next = node->next;
	node->next = newNode;
}

void *removeAfterSLNode(SLList_t *list, SLNode_t *node){
	list->len--;
	SLNode_t *removedNode = node->next;
	node->next = node->next->next;
	return freeSLNode(removedNode);
}

static SLNode_t *createSLNode(void *data){
	SLNode_t *node = malloc(sizeof(SLNode_t));
	node->data = data;
	node->next = NULL;
	return node;
}

static void *freeSLNode(SLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}
