#include <stdio.h>
#include <stdlib.h>

#include "doubly_linked_list.h"

DLList_t *createSLList(void (*freeData)(void *data)){
}

void *freeSLList(DLList_t *list){
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

int main(){
	puts("Hello world.");
	return EXIT_SUCCESS;
}
