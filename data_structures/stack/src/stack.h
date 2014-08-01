/*
	@brief An implementation of a stack data-structure using a
	singly-linked-list.
*/

#pragma once

#include "singly_linked_list.h"

typedef SLList_t Stack_t;

/*
	@brief Allocate a ::Stack_t.

	@param freeData The function that will be used to deallocate the data
		stored inside the new stack's nodes.

	@return The newly created ::Stack_t.
*/
Stack_t *createStack(void (*freeData)(void *data));

/*
	@brief Deallocate a ::Stack_t.

	@param stack Free all memory directly and indirectly consumed by the stack
		(eg the stack itself, its nodes).
*/
void freeStack(Stack_t *stack);
void *peek(Stack_t *stack);
void *pop(Stack_t *stack);
void push(Stack_t *stack, void *data);
