/*
	Functions to test the functions in `src/circular_singly_linked_list.c`.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "src/circular_singly_linked_list.h"

/*
 * @brief Deallocates data stored in the test ::CSLList_t's ::CSLNode_t.
 *
 * @param data Assumed to be a `char *`.
*/
static void freeData(void *data);

/*
 * @brief Tests the functions defined in `src/circular_singly_linked_list.h`.
*/
static void testCSLList(void);

static void freeData(void *data){
	free((char *)data);
}

static void testCSLList(void){
	CSLList_t *list = createCSLList(freeData);
	int numStrings = 9;
	char *strings[numStrings];
	int ind;
	for(ind = 0; ind < numStrings; ind++){
		strings[ind] = malloc(2);
		sprintf(strings[ind], "%d", ind);
	}

	insertCSLListHead(list, strings[0]);
	insertCSLListHead(list, strings[1]);
	insertCSLListHead(list, strings[2]);
	insertCSLListHead(list, strings[3]);
	insertCSLListHead(list, strings[4]);
	insertCSLListHead(list, strings[5]);
	insertCSLListHead(list, strings[6]);
	freeData(removeCSLListHead(list));
	freeData(removeCSLListHead(list));
	freeData(removeCSLListHead(list));
	freeData(removeCSLListHead(list));
	freeData(removeCSLListHead(list));
	insertAtIndex(list, 2, strings[8]);
	insertAtIndex(list, 3, strings[7]);
	freeData(removeAtIndex(list, 1));
	freeData(removeCSLNode(list, list->head));

	printCSLList(list, "%s\n");
	freeCSLList(list);
}

int main(){
	testCSLList();
	return EXIT_SUCCESS;
}
