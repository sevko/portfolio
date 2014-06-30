/*
 * @brief Tests the functions inside `src/singly_linked_list.c`.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "src/singly_linked_list.h"

/*
 * @brief Deallocates data stored in the test ::SLList_t's ::SLNode_t.
 *
 * @param data Assumed to be a `char *`.
*/
static void freeData(void *data);

/*
 * @brief Prints an ::SLList_t.
 *
 * Prints the list's ::SLNode_t's ::data, starting at its ::head and ending at
 * its ::tail.
 *
 * @param list Assumed to contain `char *` data.
*/
static void printSLList(SLList_t *list);

/*
 * @brief Tests the functions defined in `src/singly_linked_list.h`.
*/
static void testSLList(void);

static void freeData(void *data){
	free((char *)data);
}

static void printSLList(SLList_t *list){
	SLNode_t *currNode = list->head;
	puts("Printing singly-linked list.");
	while(currNode != NULL){
		printf("%s\n", (char *)currNode->data);
		currNode = currNode->next;
	}
	puts("Finished.");
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
	insertSLListHead(list, s7);
	insertAfterSLNode(list, list->tail, s6);
	insertAfterSLNode(list, list->tail, s5);
	insertAfterSLNode(list, list->tail, s4);
	insertAfterSLNode(list, list->tail, s3);
	insertAfterSLNode(list, list->tail, s2);
	insertAfterSLNode(list, list->tail, s1);
	insertAfterSLNode(list, list->tail, s0);
	freeData(removeSLListHead(list));
	freeData(removeSLNode(list, list->head));

	printSLList(list);
	freeSLList(list);
}

int main(){
	testSLList();
	return EXIT_SUCCESS;
}
