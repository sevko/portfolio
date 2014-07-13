#include <stdio.h>
#include <stdlib.h>

#include "doubly_linked_list.h"

// A doubly-linked node that composes a ::DLList_t.
struct DLNode {
	void *data; // The contained data.
	struct DLNode *next, // The next node in the sequence.
		*prev; // The previous node in the sequence.
};

/*
 * @brief Allocate a ::DLNode_t.
 *
 * @param data The new node's ::data member.
 *
 * @return A pointer to the newly allocated node.
*/
static DLNode_t *createDLNode(void *data);

/*
 * @brief Deallocate a ::DLNode_t.
 *
 * @param node The node to be freed. Only the node itself will be freed -- not
 *      its allocated members (namely ::data)!
 *
 * @return The argument node's ::data, to be freed at the user's discretion.
*/
static void *freeDLNode(DLNode_t *node);

DLList_t *createDLList(void (*freeData)(void *data)){
	DLList_t *list = malloc(sizeof(DLList_t));
	list->head = list->tail = NULL;
	list->len = 0;
	list->freeData = freeData;
	return list;
}

void freeDLList(DLList_t *list){
	DLNode_t *currNode = list->head;
	while(currNode){
		DLNode_t *nextNode = currNode->next;
		list->freeData(freeDLNode(currNode));
		currNode = nextNode;
	}

	free(list);
}

void insertHead(DLList_t *list, void *data){
	list->len++;
	DLNode_t *newHead = createDLNode(data);
	newHead->next = list->head;
	list->head = newHead;

	if(list->len == 1)
		list->tail = newHead;
	else
		newHead->next->prev = newHead;
}

void *removeHead(DLList_t *list){
	switch(list->len){
		case 0:
			return NULL;

		case 1:
			list->tail = NULL;
			break;

		default:
			list->head->next->prev = NULL;
	}

	list->len--;

	DLNode_t *head = list->head;
	list->head = head->next;
	return freeDLNode(head);
}

void insertTail(DLList_t *list, void *data){
	list->len++;
	DLNode_t *newTail = createDLNode(data);
	newTail->prev = list->tail;
	list->tail = newTail;

	if(list->len == 1)
		list->head = list->tail;
	else
		newTail->prev->next = newTail;
}

void *removeTail(DLList_t *list){
	switch(list->len){
		case 0:
			return NULL;

		case 1:
			list->head = NULL;
			break;

		default:
			list->tail->prev->next = NULL;
	}

	list->len--;

	DLNode_t *tail = list->tail;
	list->tail = tail->prev;
	return freeDLNode(tail);
}

void insertDLLNodeAtIndex(DLList_t *list, void *data, int ind){
	if(ind == 0)
		return insertHead(list, data);

	else if(ind == list->len)
		return insertTail(list, data);

	DLNode_t *currNode = list->head,
		*newNode = createDLNode(data);
	int currInd = 0;
	while(currInd < ind - 1){
		currNode = currNode->next;
		currInd++;
	}

	newNode->prev = currNode;
	newNode->next = currNode->next;
	currNode->next = newNode;
	list->len++;
}

void *removeDLLNodeAtIndex(DLList_t *list, int ind){
	if(ind == 0)
		return removeHead(list);

	else if(ind == list->len - 1)
		return removeTail(list);

	DLNode_t *currNode = list->head;
	int currInd = 0;
	while(currInd < ind - 1){
		currNode = currNode->next;
		currInd++;
	}

	DLNode_t *removedNode = currNode->next;
	currNode->next = currNode->next->next;
	currNode->next->prev = currNode;
	list->len--;
	return freeDLNode(removedNode);
}

void printDLList(const DLList_t * list, const char *fmt){
	DLNode_t *currNode = list->head;
	puts("Printing doubly-linked list.");
	while(currNode != NULL){
		printf(fmt, currNode->data);
		currNode = currNode->next;
	}
	puts("Finished.");
}

static DLNode_t *createDLNode(void *data){
	DLNode_t *node = malloc(sizeof(DLNode_t));
	node->data = data;
	node->prev = node->next = NULL;
	return node;
}

static void *freeDLNode(DLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}
