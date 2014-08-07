#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

Stack_t *createStack(void (*freeData)(void *data)){
	return createSLList(freeData);
}

void freeStack(Stack_t *stack){
	freeSLList(stack);
}

void *peek(Stack_t *stack){
	return stack->head->data;
}

void *pop(Stack_t *stack){
	return removeSLListHead(stack);
}

void push(Stack_t *stack, void *data){
	insertSLListHead(stack, data);
}
