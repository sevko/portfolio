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

/*
	@brief Return the data contained in the top node of the ::Stack_t.

	@param stack A stack.

	@return The ::data of the ::head of `stack`'s singly-linked-list.
*/
void *peek(Stack_t *stack);

/*
	@brief Remove the top node of a ::Stack_t.

	@param stack A stack. Must have at least one element.

	@return The ::data of the removed ::head of `stack`'s singly-linked-list.
*/
void *pop(Stack_t *stack);

/*
	@brief Insert a new node on top of a ::Stack_t.

	@param stack The stack to insert onto. Must have at least one element.
	@param data The data to be contained in the new top node of `stack`.
*/
void push(Stack_t *stack, void *data);
