#pragma once

#include "singly_linked_list.h"

typedef SLList_t Queue_t;

Queue_t *createQueue(void (*freeData)(void *data));
void freeQueue(Queue_t *queue);
void *peek(Queue_t *queue);
void enqueue(Queue_t *queue, void *data);
void *dequeue(Queue_t *queue);
