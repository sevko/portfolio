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
	printCSLList(list, "%s");
	freeCSLList(list);
}

int main(){
	testCSLList();
	return EXIT_SUCCESS;
}
