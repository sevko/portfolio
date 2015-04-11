#include <stdlib.h>

#include "array_queue.h"

struct ArrayQueue {
	void **head, **tail;
	int numElements;

	void **blockBeginning;
	int blockLength;
};

ArrayQueue_t *ArrayQueue_create(void){
	ArrayQueue_t *queue = malloc(sizeof(ArrayQueue_t));;

	int blockLength = 2;
	void *blockBeginning = malloc(blockLength * sizeof(void *));
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
	*(queue->tail++) = item;
	queue->numElements++;

	if(queue->tail == queue->blockBeginning + queue->blockLength){
		queue->blockLength *= 2;
		void **originalBlockBeginning = queue->blockBeginning;
		queue->blockBeginning = realloc(queue->blockBeginning, queue->blockLength * sizeof(void *));
		queue->head = queue->head - originalBlockBeginning + queue->blockBeginning;
		queue->tail = queue->tail - originalBlockBeginning + queue->blockBeginning;
	}
}

void *ArrayQueue_dequeue(ArrayQueue_t *queue){
	void *item = *(queue->head++);
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
