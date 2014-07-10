/*
 * @file
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

DLList_t *createSLList(void (*freeData)(void *data));
void *freeSLList(DLList_t *list);
void insertHead(DLList_t *list, const void *data);
void *removeHead(DLList_t *list);
void insertTail(DLList_t *list, const void *data);
void *removeTail(DLList_t *list);
void insertDLLNodeAtIndex(DLList_t *list, void *data, int ind);
void *removeDLLNodeAtIndex(DLList_t *list, int ind);
