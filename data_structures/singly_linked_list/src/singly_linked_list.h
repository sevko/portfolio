/*!
 * @file
 * @brief An implementation of a singly-linked-list.
*/

#pragma once

#define isHead(list, node) (void)
#define isTail(list, node) (void)

// A singly-linked node that composes a ::SLList_t.
typedef struct SLNode {
	void *data; // The contained data.
	struct SLNode *next; // The next node in the sequence.
} SLNode_t;

// A singly-linked-list.
typedef struct {
	int len; // The number of ::SLNode_t inside the list.
	SLNode_t *head, // The head of the list.
		*tail; // The tail of the list.
	void (*freeData)(void *data);
} SLList_t;

/*
 * @brief Allocate an ::SLList_t.
 *
 * @param freeData A function to free the data to be stored inside ::SLNode_t
 *      inside the returned ::SLList_t.
 *
 * @return A pointer to the new ::SLList_t.
*/
SLList_t *createSLList(void (*freeData)(void *data));

/*
 * @brief Deallocate an ::SLList_t.
 *
 * @param list The list to be free'd.
*/
void freeSLList(SLList_t *list);
void insertAfterSLNode(SLList_t *list, SLNode_t *node);
void removeAfterSLNode(SLList_t *list, SLNode_t *node);
