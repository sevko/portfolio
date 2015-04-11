#include <stdlib.h>

#include "array_queue.h"

struct ArrayQueue {
	void **head, **tail;
	int numElements;

	void **blockBeginning;
	size_t blockLength;
};

ArrayQueue_t *ArrayQueue_create(void){
	ArrayQueue_t *queue = malloc(sizeof(ArrayQueue_t));;

	size_t blockLength = 10 * sizeof(void *);
	void *blockBeginning = malloc(blockLength);
	*queue = (ArrayQueue_t){
		.head = blockBeginning,
		.tail = blockBeginning,
		.numElements = 0,
		.blockBeginning = blockBeginning,
		.blockLength = blockLength
	};
	return queue;
}

void ArrayQueue_destroy(ArrayQueue_t *queue){
	free(queue->blockBeginning);
	free(queue);
}

void ArrayQueue_enqueue(ArrayQueue_t *queue, void *item){
	*queue->tail = item;
	queue->tail++;
	queue->numElements++;

	/* resize logic */
}

void *ArrayQueue_dequeue(ArrayQueue_t *queue){
	void *item = *queue->head;
	queue->head++;
	queue->numElements--;
	return item;
}

void *ArrayQueue_peek(ArrayQueue_t *queue){
	return *queue->head;
}

int ArrayQueue_size(ArrayQueue_t *queue){
	return queue->numElements;
}

bool ArrayQueue_empty(ArrayQueue_t *queue){
	return queue->numElements == 0;
}
