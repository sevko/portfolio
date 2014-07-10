#include <stdio.h>
#include <stdlib.h>

#include "doubly_linked_list.h"

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

void *freeDLNode(DLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}

void insertHead(DLList_t *list, const void *data){
}

void *removeHead(DLList_t *list){
}

void insertTail(DLList_t *list, const void *data){
}

void *removeTail(DLList_t *list){
}

void insertDLLNodeAtIndex(DLList_t *list, void *data, int ind){
}

void *removeDLLNodeAtIndex(DLList_t *list, int ind){
}
