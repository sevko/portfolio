#include <stdio.h>
#include <stdlib.h>

#include "queue.h"

Queue_t *createQueue(void (*freeData)(void *data)){
	return createSLList(freeData);
}

void freeQueue(Queue_t *queue){
	freeSLList(queue);
}

void *peek(Queue_t *queue){
	return queue->head->data;
}

void enqueue(Queue_t *queue, void *data){
	if(queue->len == 0)
		insertSLListHead(queue, data);
	else
		insertAfterSLNode(queue, queue->tail, data);
}

void *dequeue(Queue_t *queue){
	return removeSLListHead(queue);
}
