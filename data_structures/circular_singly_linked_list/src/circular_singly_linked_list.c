#include <stdio.h>
#include <stdlib.h>

#include "circular_singly_linked_list.h"

// A singly-linked node that composes an ::CSLList_t.
struct CSLNode {
	void *data; // The contained data.
	struct CSLNode *next; // The next node in the sequence.
};

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
		printf(nodeDataFmt, currNode);
		currNode = currNode->next;
		ind++;
	}

	puts("Finished printing.");
}

static void *freeCSLNode(CSLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}
