/*
 * @brief Functions to test the implementation of a doubly-linked list.
*/

#include <stdio.h>
#include <stdlib.h>

#include "src/doubly_linked_list.h"

/*
 * @brief Tests the ::DLList_t functions.
*/
static void testDLList(void);

/*
 * @brief Deallocates the ::data inside these test-cases' ::DLNode_t.
 *
 * Used in initializing this file's test ::DLList_t.
 *
 * @param data The data to deallocate; assumed to be a `char *`.
*/
static void freeData(void *data);

static void testDLList(void){
	int numStrings = 9;
	char *strings[numStrings];
	int ind;
	for(ind = 0; ind < numStrings; ind++){
		strings[ind] = malloc(2);
		sprintf(strings[ind], "%d", ind);
	}

	DLList_t *list = createDLList(freeData);
	insertDLLNodeAtIndex(list, strings[0], 0);
	insertDLLNodeAtIndex(list, strings[1], 0);
	insertDLLNodeAtIndex(list, strings[2], 0);
	insertDLLNodeAtIndex(list, strings[3], 0);
	insertDLLNodeAtIndex(list, strings[4], 0);
	insertDLLNodeAtIndex(list, strings[5], 0);
	insertDLLNodeAtIndex(list, strings[6], 0);
	insertDLLNodeAtIndex(list, strings[7], 0);
	insertDLLNodeAtIndex(list, strings[8], 0);
	printDLList(list, "%s\n");
	freeDLList(list);
}

static void freeData(void *data){
	free((char *)data);
}

int main(){
	testDLList();
	return EXIT_SUCCESS;
}
