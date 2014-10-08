/**
 * @brief An implementation of a binary tree.
*/

#include <stdbool.h>

// One node of the binary tree.
typedef struct BinaryTree_Node {
	void *data; // The data contained inside this node.
	struct BinaryTree_Node *left, // The left child.
		*right; //  The right child.
} BinaryTree_Node_t;

// A binary tree.
typedef struct {
	int size; // The total number of nodes inside this tree.

	// A function used to free the data items stored inside this tree's nodes.
	void (*freeData)(void *data);
	BinaryTree_Node_t *root; // The root node of the tree.
} BinaryTree_Tree_t;

/**
 * @brief Allocate memory for a new BinaryTree_Tree_t.
 * @param freeData A function to free the data items (of arbitrary type) to be
 *      contained inside this tree's nodes.
 * @return A pointer to the new, empty tree.
*/
BinaryTree_Tree_t *BinaryTree_create(void (*freeData)(void *data));

/**
 * @brief Create and insert a root node into an empty tree.
 * @param tree Must be a newly instantiatet tree, with a `size` of 0.
 * @param data The `data` member of the newly created root node.
*/
void BinaryTree_insertRoot(BinaryTree_Tree_t *tree, void *data);

/*
 * @param tree The tree to deallocate (along with all its nodes).
*/
void BinaryTree_free(BinaryTree_Tree_t *tree);

/**
 * @brief Create and insert a node as the left child of another node.
 * @param tree The tree containing 'node'.
 * @param node The node into whose `left` position a new node will be
 *      inserted; it'll inherit the current value of `left`.
 * @param data The `data` member of the newly created node.
*/
void BinaryTree_insertLeft(
	BinaryTree_Tree_t *tree, BinaryTree_Node_t *node, void *data
);

/**
 * See `BinaryTree_insertLeft`: works the same way, but operates on nodes'
 * `right` member instead of their `left`.
*/
void BinaryTree_insertRight(
	BinaryTree_Tree_t *tree, BinaryTree_Node_t *node, void *data
);

/**
 * @brief Recursively remove a node.
 * @param tree The tree containing `node`. Size is decremented accordingly.
 * @param node The node to deallocate completely (including its `data`), along
 *      with all of its children nodes.
*/
void BinaryTree_removeNode(BinaryTree_Tree_t *tree, BinaryTree_Node_t *node);

/*
 * @param node A node.
 * @return Whether or not `node` is a leaf (ie, has no children).
*/
bool BinaryTree_isLeaf(const BinaryTree_Node_t *node);

/*
 * @brief Perform a pre-order traversal of a tree.
 * @param node The node to begin traversing from.
 * @param action The function to pass each traversed node's `data` item to,
 *      before the traversal recurses down that node's `left` and `right`
 *      children.
*/
void BinaryTree_preOrder(
	const BinaryTree_Node_t *node, void action(void *data)
);

/*
 * @brief Perform an in-order traversal of a tree.
 * @param node The node to begin traversing from.
 * @param action The function to pass each traversed node's `data` item to,
 *      after the traversal recursed down that node's `left` child, but not its
 *      right.
*/
void BinaryTree_inOrder(
	const BinaryTree_Node_t *node, void action(void *data)
);

/*
 * @brief Perform a post-order traversal of a tree.
 * @param node The node to begin traversing from.
 * @param action The function to pass each traversed node's `data` item to,
 *      after the traversal recursed down that node's `left` and `right`
 *      children.
*/
void BinaryTree_postOrder(
	const BinaryTree_Node_t *node, void action(void *data)
);
