/**
 * A queue implemented using an array (read: a contiguous block of memory on
 * the heap) rather than a singly-linked list.
 */

#pragma once

#include <stdbool.h>

typedef struct ArrayQueue ArrayQueue_t;

ArrayQueue_t *ArrayQueue_create(void);
void ArrayQueue_destroy(ArrayQueue_t *queue);
void ArrayQueue_enqueue(ArrayQueue_t *queue, void *item);
void *ArrayQueue_dequeue(ArrayQueue_t *queue);
void *ArrayQueue_peek(ArrayQueue_t *queue);
int ArrayQueue_size(ArrayQueue_t *queue);
bool ArrayQueue_empty(ArrayQueue_t *queue);
