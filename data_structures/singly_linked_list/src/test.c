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
	int numStrings = 9;
	char *strings[numStrings];
	int ind;
	for(ind = 0; ind < numStrings; ind++){
		strings[ind] = malloc(2);
		sprintf(strings[ind], "%d", ind);
	}

	SLList_t *list = createSLList(freeData);
	insertSLListHead(list, strings[7]);
	insertAfterSLNode(list, list->tail, strings[6]);
	insertAfterSLNode(list, list->tail, strings[5]);
	insertAfterSLNode(list, list->tail, strings[4]);
	insertAfterSLNode(list, list->tail, strings[3]);
	insertAfterSLNode(list, list->tail, strings[2]);
	insertAfterSLNode(list, list->tail, strings[1]);
	insertAfterSLNode(list, list->tail, strings[0]);
	freeData(removeSLListHead(list));
	freeData(removeSLNode(list, list->head));
	insertAtIndex(list, 0, strings[8]);
	insertAtIndex(list, list->len, strings[8]);

	printSLList(list);
	freeSLList(list);
}

int main(){
	testSLList();
	return EXIT_SUCCESS;
}
