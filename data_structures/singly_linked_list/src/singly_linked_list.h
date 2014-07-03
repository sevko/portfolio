/*!
 * @file
 * @brief An implementation of a singly-linked-list.
*/

#pragma once

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
	// Function to deallocate the data stored inside this list's ::SLNode_t.
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
 * @param list The list to lose its current ::head. Must have ::len greater
 * than 0.
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
 * @param node The node whose ::SLNode_t::next will be removed. Cannot be
 *      list's ::tail.
 *
 * @return The removed node's ::data.
*/
void *removeAfterSLNode(SLList_t *list, SLNode_t *node);

/*
 * @brief Inserts an ::SLNode_t at an ::SLList_t index.
 *
 * @param list The list to insert a new ::SLNode_t into.
 * @param index The index of `list` into which the new ::SLNode_t will be
 *      inserted. Indexing begins at 0, and ends at `list->len` (unlike in
 *      primitive array types in most languages, where it'd end at
 *      `list->len - 1`). Given the following list:
 *
 *          0    1    2    3
 *          a -> b -> d -> NULL
 *
 *      Inserting "c" at index 2 would result in:
 *
 *          0    1    2    3    4
 *          a -> b -> c -> d -> NULL
 *
 *      Inserting "c" at index 3 would have resulted in:
 *
 *          0    1    2    3    4
 *          a -> b -> d -> c -> NULL
 *
 * @param data The ::data to be contained inside the ::SLNode_t that will be
 *      created and inserted into `list`.
*/
void insertAtIndex(SLList_t *list, int index, void *data);

/*
 * @brief Removes an ::SLNode_t from an ::SLList_t index.
 *
 * @param list The list to remove an ::SLNode_t from.
 * @param index The index in `list` from which an ::SLNode_t will be removed.
 *      Indexing begins at 0 and ends at (len->list - 1) -- other values will
 *      be safely handled, and no ::SLNode_t will be removed. Example:
 *
 *      Given the following list:
 *
 *          0    1    2    3
 *          a -> b -> d -> NULL
 *
 *      Removing the ::SLNode_t at index 1 would result in:
 *
 *          0    1    2
 *          a -> d -> NULL
 *
 * @return If a valid 'index' was supplied, the ::data of the removed
 *      ::SLNode_t; otherwise, NULL.
*/
void *removeAtIndex(SLList_t *list, int index);

/*
 * @brief Remove a ::SLNode_t from a ::SLList_t.
 *
 * @param list A list.
 * @param node The node to be removed. Must exist within `list`.
 *
 * @return A pointer to the removed ::SLNode_t.
*/
void *removeSLNode(SLList_t *list, SLNode_t *node);
