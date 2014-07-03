#include <stdio.h>
#include <stdlib.h>

#include "singly_linked_list.h"

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

	if(list->len == 1)
		list->tail = newHead;
}

void *removeSLListHead(SLList_t *list){
	list->len--;
	SLNode_t *head = list->head;
	list->head = list->head->next;
	return freeSLNode(head);
}

void insertAfterSLNode(SLList_t *list, SLNode_t *node, void *data){
	list->len++;
	SLNode_t *newNode = createSLNode(data);
	newNode->next = node->next;
	node->next = newNode;

	if(node == list->tail)
		list->tail = newNode;
}

void *removeAfterSLNode(SLList_t *list, SLNode_t *node){
	list->len--;
	SLNode_t *removedNode = node->next;
	node->next = node->next->next;

	if(removedNode == list->tail)
		list->tail = node;

	return freeSLNode(removedNode);
}

void insertAtIndex(SLList_t *list, int index, void *data){
	if(0 < index && index < list->len){
		SLNode_t *currNode = list->head;
		int ind;
		for(ind = 0; ind < index; ind++)
			currNode = currNode->next;

		insertAfterSLNode(list, currNode, data);
	}
	else if(index == 0)
		insertSLListHead(list, data);
	else if(index == list->len)
		insertAfterSLNode(list, list->tail, data);
	return;
}

void *removeAtIndex(SLList_t *list, int index){
	if(0 < index && index < list->len){
		SLNode_t *currNode = list->head;
		int ind;
		for(ind = 0; ind < index - 1; ind++)
			currNode = currNode->next;

		return removeAfterSLNode(list, currNode);
	}
	else if(index == 0)
		return removeSLListHead(list);
	return NULL;
}

void *removeSLNode(SLList_t *list, SLNode_t *node){
	if(node == list->head)
		return removeSLListHead(list);

	SLNode_t *currNode = list->head;
	while(currNode->next != node)
		currNode = currNode->next;

	list->len--;
	currNode->next = node->next;
	return freeSLNode(node);
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
