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
	DLList_t *list = createDLList(freeData);
	freeDLList(list);
}

static void freeData(void *data){
	free((char *)data);
}

int main(){
	testDLList();
	return EXIT_SUCCESS;
}
