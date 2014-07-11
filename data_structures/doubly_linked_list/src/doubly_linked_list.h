/*
 * @brief An implementation of a doubly-linked list.
*/

#pragma once

// A doubly-linked node that composes a ::DLList_t.
typedef struct DLNode {
	void *data; // The contained data.
	struct DLNode *next, // The next node in the sequence.
		*prev; // The previous node in the sequence.
} DLNode_t;

// A doubly-linked-list, composed of ::DLNode_t.
typedef struct {
	int len; // The number of nodes inside the list.
	DLNode_t *head, // The first node in the list.
		*tail; // The last node in the list.
	// Function to deallocate the data stored inside this list's nodes.
	void (*freeData)(void *data);
} DLList_t;

/*
 * @brief Allocate an empty doubly-linked list.
 *
 * @return A pointer to the list. ::next and ::prev will be `NULL`, and ::len
 *      `0`.
*/
DLList_t *createDLList(void (*freeData)(void *data));

/*
 * @brief Deallocate a ::DLList_t.
 *
 * @param list The list to free. All associated memory (meaning member
 *      ::DLNode_t) will be deallocated.
*/
void freeDLList(DLList_t *list);

/*
 * @brief Allocate a ::DLNode_t.
 *
 * @param data The new node's ::data member.
 *
 * @return A pointer to the newly allocated node.
*/
DLNode_t *createDLNode(void *data);

/*
 * @brief Deallocate a ::DLNode_t.
 *
 * @param node The node to be freed. Only the node itself will be freed -- not
 *      its allocated members (namely ::data)!
 *
 * @return The argument node's ::data, to be freed at the user's discretion.
*/
void *freeDLNode(DLNode_t *node);

/*
 * @brief Create and insert a new head ::DLNode_t into a ::DLList_t.
 *
 * @param list The list to insert a new head into. May be of any length (edge
 *      cases, like a length of `0`, are handled appropriately).
 * @param data The ::data of the new ::head ::DLNode_t.
*/
void insertHead(DLList_t *list, void *data);

/*
 * @brief Remove a ::DLList_t ::head.
 *
 * @param list The list to remove the leading node from. Can be of any length.
 *
 * @return If the length of the list is 0, NULL. Otherwise, the ::data of the
 *      removed ::head (the ::DLNode_t itself is deallocated).
*/
void *removeHead(DLList_t *list);

/*
 * @brief Create and insert a new ::tail into a ::DLList_t.
 *
 * @param list The list to insert into.
 * @param data The ::data of the new tail.
*/
void insertTail(DLList_t *list, void *data);

/*
 * @brief Remove a ::DLList_t ::tail.
 *
 * @param list The list to remove the last node from. Can be of any length.
 *
 * @return If the length of the list is 0, NULL. Otherwise, the ::data of the
 *      removed ::tail (the ::DLNode_t itself is deallocated).
*/
void *removeTail(DLList_t *list);

void insertDLLNodeAtIndex(DLList_t *list, void *data, int ind);
void *removeDLLNodeAtIndex(DLList_t *list, int ind);
