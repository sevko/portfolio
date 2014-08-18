#pragma once

#include "singly_linked_list.h"

typedef SLList_t Queue_t;

/*
	@brief Allocate a ::Queue_t.

	@param freeData The function that will be used to deallocate the data
		stored inside the new queue's nodes.

	@return The newly created ::Queue_t.
*/
Queue_t *createQueue(void (*freeData)(void *data));

/*
	@brief Deallocate a ::Queue_t.

	@param queue Free all memory directly and indirectly consumed by the queue
		(eg the queue itself, its nodes).
*/
void freeQueue(Queue_t *queue);

/*
	@brief Return the data contained in the front node of the ::Queue_t.

	@param queue A queue.

	@return The ::data of the ::head of `queue`'s singly-linked-list.
*/
void *peek(Queue_t *queue);

/*
	@brief Insert data into the back of a queue.

	@param queue The new node will be inserted at the end of this queue.
	@param data The data to be contained in the new node.
*/
void enqueue(Queue_t *queue, void *data);

/*
	@brief Remove a node from the front of a queue.

	@param queue The first node of this queue will be removed.

	@return The data of `queue's` removed (and itself deallocated) front node.
*/
void *dequeue(Queue_t *queue);
