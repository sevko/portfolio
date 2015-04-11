/**
 * A queue implemented using an array (read: a contiguous block of memory on
 * the heap) rather than a singly-linked list.
 */

#pragma once

#include <stdbool.h>

typedef struct ArrayQueue ArrayQueue_t;

/**
 * Allocate an ArrayQueue_t and return a pointer to it. Must be deallocate with
 * `ArrayQueue_destroy()` after use.
 */
ArrayQueue_t *ArrayQueue_create(void);

/*
 * Clean up all memory associated with a `queue` allocated via
 * `ArrayQueue_create()`.
 */
void ArrayQueue_destroy(ArrayQueue_t *queue);

/**
 * Enqueue `item` into `queue`. Amortized time is O(1).
 */
void ArrayQueue_enqueue(ArrayQueue_t *queue, void *item);

/**
 * Dequeue an item from `queue`. Must be called if `!ArrayQueue_empty()`. O(1)
 */
void *ArrayQueue_dequeue(ArrayQueue_t *queue);

/**
 * Like `ArrayQueue_dequeue()`, but only return a pointer to the to-be-dequeued
 * element without actually dequeueing it. O(1)
 */
void *ArrayQueue_peek(ArrayQueue_t *queue);

/**
 * Like `ArrayQueue_peek()`, but return a pointer to the `index`th element
 * in `queue`. `index` must be 0 <= `index` < ArrayQueue_size(), with indexing
 * beginning at the queue's head (ie `ArrayQueue_peekIndex(queue, 0)` is the
 * same as `ArrayQueue_peek(queue)`). O(1)
 */
void *ArrayQueue_peekIndex(ArrayQueue_t *queue, int index);

/**
 * Return the number of elements stored inside `queue`.
 */
int ArrayQueue_size(ArrayQueue_t *queue);

/**
 * Whether or not `queue` is empty.
 */
bool ArrayQueue_empty(ArrayQueue_t *queue);
