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

static void freeData(void *data){
	free((char *)data);
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
	insertAtIndex(list, 0, strings[8]);

	printSLList(list, "%s\n");
	freeSLList(list);
}

int main(){
	testSLList();
	return EXIT_SUCCESS;
}
