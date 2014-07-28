#include <stdio.h>
#include <stdlib.h>

#include "circular_singly_linked_list.h"

// A singly-linked node that composes an ::CSLList_t.
// struct CSLNode {
	// void *data; // The contained data.
	// struct CSLNode *next; // The next node in the sequence.
// };

static CSLNode_t *createCSLNode(void *data);

/*
	@brief Deallocate a ::CSLNode_t.

	@param node The node to free. Its ::data will NOT be freed.

	@return The ::data of the now deallocated `node`.
*/
static void *freeCSLNode(CSLNode_t *node);

CSLList_t *createCSLList(void (*freeData)(void *data)){
	CSLList_t *list = malloc(sizeof(CSLList_t));
	list->head = NULL;
	list->len = 0;
	list->freeData = freeData;
	return list;
}

void freeCSLList(CSLList_t *list){
	CSLNode_t *currNode = list->head;
	int ind = 0;

	/*
		Traversal based on an incremented index and ::CSLList_t::len is
		preferred to comparing a current ::CSLNode_t against the next, and then
		setting it to the ::next node, because of various edge-cases specific
		to circular lists.
	*/
	while(ind < list->len){
		CSLNode_t *nextNode = currNode->next;
		list->freeData(freeCSLNode(currNode));
		currNode = nextNode;
		ind++;
	}

	free(list);
}

void insertCSLListHead(CSLList_t *list, void *data){
	list->len++;
	CSLNode_t *newHead = createCSLNode(data);
	newHead->next = list->head;
	list->head = newHead;
}

void *removeCSLListHead(CSLList_t *list){
	if(0 < list->len){
		list->len--;
		CSLNode_t *removedHead = list->head;
		list->head = list->head->next;
		return freeCSLNode(removedHead);
	}
	else
		return NULL;
}

void insertAfterCSLNode(CSLList_t *list, CSLNode_t *node, void *data){
	list->len++;
	CSLNode_t *newNode = createCSLNode(data);
	newNode->next = node->next;
	node->next = newNode;
}

void *removeAfterCSLNode(CSLList_t *list, CSLNode_t *node){
	list->len--;
	CSLNode_t *removedNode = node->next;
	node->next = node->next->next;
	return freeCSLNode(removedNode);
}

void printCSLList(const CSLList_t *list, const char *nodeDataFmt){
	puts("Printing circular-singly-linked-list.");

	/*
		Traversal based on an incremented index and ::CSLList_t::len is
		preferred to comparing a current ::CSLNode_t against the next, and then
		setting it to the ::next node, because of various edge-cases specific
		to circular lists.
	*/
	CSLNode_t *currNode = list->head;
	int ind = 0;
	while(ind < list->len){
		printf(nodeDataFmt, currNode->data);
		currNode = currNode->next;
		ind++;
	}

	puts("Finished printing.");
}

static CSLNode_t *createCSLNode(void *data){
	CSLNode_t *node = malloc(sizeof(CSLNode_t));
	node->data = data;
	node->next = NULL;
	return node;
}

static void *freeCSLNode(CSLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}
