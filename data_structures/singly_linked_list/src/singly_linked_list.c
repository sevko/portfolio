#include <stdio.h>
#include <stdlib.h>

#include "singly_linked_list.h"

/*
 * @brief Deallocate an ::SLNode_t
 *
 * @param node A node.
 *
 * @return The deallocated node's ::data.
*/
static void *freeSLNode(SLNode_t *node);

SLList_t *createSLList(void (*freeData)(void *data)){
	SLList_t *list = malloc(sizeof(SLList_t));
	list->head = list->tail = NULL;
	list->len = 0;
	return list;
}

void freeSLList(SLList_t *list){
	SLNode_t *currNode = list->head;
	while(currNode != NULL){
		SLNode_t *nextNode = currNode->next;
		list->freeData(freeSLNode(currNode));
		currNode = nextNode;
	}
}

void insertAfterSLNode(SLList_t *list, SLNode_t *node){
}

void removeAfterSLNode(SLList_t *list, SLNode_t *node){
}

static void *freeSLNode(SLNode_t *node){
	void *data = node->data;
	free(node);
	return data;
}

int main(){
	puts("Hello world.");
	return EXIT_SUCCESS;
}
