/*
	@brief Test the stack implemented in `stack.c`.
*/

#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

/*
 * @brief Deallocates data stored in the test ::Stack_t's nodes.
 *
 * @param data Assumed to be a `char *`.
*/
static void freeData(void *data);

/*
	@brief Test various stack functions.
*/
static void testStack(void);

static void freeData(void *data){
	free((char *)data);
}

static void testStack(void){
	Stack_t *stack = createStack(freeData);
	freeStack(stack);
}

int main(){
	testStack();
	return EXIT_SUCCESS;
}
