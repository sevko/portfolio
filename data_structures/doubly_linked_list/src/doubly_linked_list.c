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
}

void *removeDLLNodeAtIndex(DLList_t *list, int ind){
}
