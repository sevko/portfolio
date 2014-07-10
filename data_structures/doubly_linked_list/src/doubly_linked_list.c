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

DLNode_t *createDLNode(void *data){
	DLNode_t *node = malloc(sizeof(DLNode_t));
	node->data = data;
	return data;
}

void *freeDLNode(DLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}

void insertHead(DLList_t *list, void *data){
	list->len++;
	DLNode_t *newHead = createDLNode(data);
	newHead->next = list->head;
	list->head = newHead;

	if(list->len == 1)
		list->tail = newHead;
}

void *removeHead(DLList_t *list){
}

void insertTail(DLList_t *list, void *data){
}

void *removeTail(DLList_t *list){
}

void insertDLLNodeAtIndex(DLList_t *list, void *data, int ind){
}

void *removeDLLNodeAtIndex(DLList_t *list, int ind){
}
