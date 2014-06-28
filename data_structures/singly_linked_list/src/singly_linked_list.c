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

/*
 * @brief Tests the other functions defined within this source file.
*/
static void testSLList(void);

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

#include <string.h>

void freeData(void *data){
	free((char *)data);
}

static void testSLList(void){
	char *s0 = malloc(20);
	strcpy(s0, "0000000000");
	char *s1 = malloc(20);
	strcpy(s1, "1111111111");
	char *s2 = malloc(20);
	strcpy(s2, "2222222222");
	char *s3 = malloc(20);
	strcpy(s3, "3333333333");
	char *s4 = malloc(20);
	strcpy(s4, "4444444444");
	char *s5 = malloc(20);
	strcpy(s5, "5555555555");
	char *s6 = malloc(20);
	strcpy(s6, "6666666666");
	char *s7 = malloc(20);
	strcpy(s7, "7777777777");

	SLList_t *list = createSLList(freeData);
	insertSLListHead(list, s0);
	insertSLListHead(list, s1);
	insertSLListHead(list, s2);
	insertSLListHead(list, s3);
	insertSLListHead(list, s4);
	insertSLListHead(list, s5);
	insertSLListHead(list, s6);
	insertSLListHead(list, s7);
	list->freeData(removeSLListHead(list));
	list->freeData(removeSLListHead(list));
	list->freeData(removeSLListHead(list));
	list->freeData(removeSLListHead(list));
	list->freeData(removeSLListHead(list));
	list->freeData(removeSLListHead(list));
	list->freeData(removeSLListHead(list));
	list->freeData(removeSLListHead(list));
	freeSLList(list);
}

int main(){
	testSLList();
	return EXIT_SUCCESS;
}
