#include <stdio.h>
#include <stdlib.h>

#include "circular_singly_linked_list.h"

// A singly-linked node that composes an ::CSLList_t.
typedef struct CSLNode {
	void *data; // The contained data.
	struct CSLNode *next; // The next node in the sequence.
} CSLNode_t;

// A singly-linked-list.
typedef struct {
	int len; // The number of ::CSLNode_t inside the list.
	CSLNode_t *head; // The beginning of the list.
	// Function to deallocate the data stored inside this list's ::CSLNode_t.
	void (*freeData)(void *data);
} CSLList_t;

void insertCSLListHead(CSLList_t *list, void *data);
void *removeCSLListHead(CSLList_t *list);
void insertAfterCSLNode(CSLList_t *list, CSLNode_t *node, void *data);
void *removeAfterCSLNode(CSLList_t *list, CSLNode_t *node);
void insertAtIndex(CSLList_t *list, int index, void *data);
void *removeAtIndex(CSLList_t *list, int index);
void *removeCSLNode(CSLList_t *list, CSLNode_t *node);
static CSLNode_t *createCSLNode(void *data);
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

	while(ind < list->len){
		CSLNode_t *nextNode = currNode->next;
		list->freeData(freeCSLNode(currNode));
		currNode = nextNode;
		ind++;
	}
}

static void *freeCSLNode(CSLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}
