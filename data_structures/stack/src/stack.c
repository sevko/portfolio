#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

Stack_t *createStack(void (*freeData)(void *data)){
	return createSLList(freeData);
}

void freeStack(Stack_t *stack){
	freeSLList(stack);
}
