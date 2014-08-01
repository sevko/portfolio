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
	int numStrings = 9;
	char *strings[numStrings];
	int ind;
	for(ind = 0; ind < numStrings; ind++){
		strings[ind] = malloc(2);
		sprintf(strings[ind], "%d", ind);
	}

	Stack_t *stack = createStack(freeData);
	push(stack, strings[0]);
	push(stack, strings[1]);
	push(stack, strings[2]);
	push(stack, strings[3]);
	push(stack, strings[4]);
	push(stack, strings[5]);
	push(stack, strings[6]);
	push(stack, strings[7]);
	push(stack, strings[8]);
	freeData(pop(stack));
	freeData(pop(stack));
	freeData(pop(stack));
	freeData(pop(stack));
	freeData(pop(stack));
	freeData(pop(stack));
	freeData(pop(stack));
	freeData(pop(stack));
	printf("%s\n", peek(stack));
	freeStack(stack);
}

int main(){
	testStack();
	return EXIT_SUCCESS;
}
