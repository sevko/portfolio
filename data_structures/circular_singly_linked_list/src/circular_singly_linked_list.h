/*
	An implementation of a circular, singly-linked list.
*/

typedef struct CSLNode CSLNode_t;

// A singly-linked-list.
typedef struct {
	int len; // The number of ::CSLNode_t inside the list.
	CSLNode_t *head; // The beginning of the list.
	// Function to deallocate the data stored inside this list's ::CSLNode_t.
	void (*freeData)(void *data);
} CSLList_t;

/*
	@brief Allocate a ::CSLList_t.

	@param freeData Function that will be used to deallocate the data contained
		inside the new list's nodes.

	@return The new list.
*/
CSLList_t *createCSLList(void (*freeData)(void *data));

/*
	@brief Deallocate a ::CSLList_t.

	@param list The list to be freed. All member nodes will also be
		deallocated.
*/
void freeCSLList(CSLList_t *list);
void insertCSLListHead(CSLList_t *list, void *data);
void *removeCSLListHead(CSLList_t *list);
void insertAfterCSLNode(CSLList_t *list, CSLNode_t *node, void *data);
void *removeAfterCSLNode(CSLList_t *list, CSLNode_t *node);
void insertAtIndex(CSLList_t *list, int index, void *data);
void *removeAtIndex(CSLList_t *list, int index);
void *removeCSLNode(CSLList_t *list, CSLNode_t *node);

/*
	@brief Prints the contents of an ::CSLList_t.

	@param list The list whose ::CSLNode_t will be printed.
	@param nodeDataFmt Since ::CSLList_t is implemented to support arbitrary
		data-types by storing `void *` data inside its ::CSLNode_t, a
		format-string is needed to print its contents. The string should be a
		standard C format string (see `printf(3)`).
*/
void printCSLList(const CSLList_t *list, const char *nodeDataFmt);
