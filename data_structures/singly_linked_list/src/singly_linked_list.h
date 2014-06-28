/*!
 * @file
 * @brief An implementation of a singly-linked-list.
*/

#pragma once

#define isSLListHead(list, node) (void)
#define isSLListTail(list, node) (void)

// A singly-linked node that composes an ::SLList_t.
typedef struct SLNode {
	void *data; // The contained data.
	struct SLNode *next; // The next node in the sequence.
} SLNode_t;

// A singly-linked-list.
typedef struct {
	int len; // The number of ::SLNode_t inside the list.
	SLNode_t *head, // The beginning of the list.
		*tail; // The end of the list.
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

/*
 * @brief Insert a new ::SLNode_t into an ::SLList_t::head.
 *
 * @param list The list to gain a new ::head.
 * @param data The new ::head's ::data.
*/
void insertSLListHead(SLList_t *list, void *data);

/*
 * @brief Remove an ::SLList_t's ::head.
 *
 * @param list The list to lost its current ::head.
 *
 * @return The removed ::head's ::data.
*/
void *removeSLListHead(SLList_t *list);

/*
 * @brief Insert an ::SLNode_t after another ::SLNode_t.
 *
 * @param list The list containing `node`.
 * @param node The node to insert a new node after.
 * @param data The data contained within the new node.
*/
void insertAfterSLNode(SLList_t *list, SLNode_t *node, void *data);

/*
 * @brief Remove an ::SLNode_t after another ::SLNode_t.
 *
 * @param list The list containing `node`.
 * @param node The node whose ::SLNode_t::next will be removed.
 *
 * @return The removed node's ::data.
*/
void *removeAfterSLNode(SLList_t *list, SLNode_t *node);
