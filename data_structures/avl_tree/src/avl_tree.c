#include <stdio.h>
#include <stdlib.h>

#include "avl_tree.h"

/*
 * @brief Supplements `AVLTree_insert()`; insert a data item into a tree.
 * @param tree The tree to insert the item into.
 * @param node The node to begin recursively searching for a candidate position
 *      from.  Unless you know what you're doing, this should invariably be
 *      `tree->root`, or the top of the tree, because `data` will then be
 *      inserted into a position after taking into account all of the existing
 *      data members, and will allow the tree search mechanism to function as
 *      intended. Must not be null.
 * @param data The data item node to insert. Its `data` member be usable by
 *      `tree->compareData()`.
 *
 * @return The change in the parent node's balance factor (either 1 or 0); the
 *      former is multiplied by 1/-1 according to whether this `_insert()` call
 *      operated on the right/left child of the parent node respectively.
*/
static int _insert(
	AVLTree_Tree_t *tree, BinaryTree_Node_t *node, AVLTree_Node_t *dataNode
);

/*
 * @brief Allocate a node.
 * @param data The `data` member of the new node.
 * @return A pointer to the new node. Its `balanceFactor` will be 0.
*/
static AVLTree_Node_t *_createNode(void *data);

/*
 * @param num A number.
 * @return The absolute value of `num`.
*/
static int _abs(int num);

AVLTree_Tree_t *AVLTree_create(
	void (*freeData)(void *data),
	int (*compareData)(const void *data1, const void *data2)
){
	AVLTree_Tree_t *tree = BinaryTree_create(freeData);
	tree->compareData = compareData;
	return tree;
}

void AVLTree_insert(AVLTree_Tree_t *tree, void *data){
	data = _createNode(data);
	if(tree->size == 0){
		BinaryTree_insertRoot(tree, data);
	}
	else {
		_insert(tree, tree->root, data);
	}
}

static int _insert(
	AVLTree_Tree_t *tree, BinaryTree_Node_t *node, AVLTree_Node_t *dataNode
){
	void *currData = ((AVLTree_Node_t *)(node->data))->data;
	int balanceFactorDelta;
	if(tree->compareData(currData, dataNode->data) < 0){
		if(node->right){
			balanceFactorDelta = _insert(tree, node->right, dataNode);
		}
		else {
			BinaryTree_insertRight(tree, node, dataNode);
			balanceFactorDelta = 1;
		}
	}
	else {
		if(node->left){
			balanceFactorDelta = -_insert(tree, node->left, dataNode);
		}
		else {
			BinaryTree_insertLeft(tree, node, dataNode);
			balanceFactorDelta = -1;
		}

	}
	int *balanceFactor = &((AVLTree_Node_t *)(node->data))->balanceFactor;
	int prevBalanceFactor = *balanceFactor;
	*balanceFactor += balanceFactorDelta;
	return (_abs(prevBalanceFactor) < _abs(*balanceFactor)) ? 1 : 0;
}

static AVLTree_Node_t *_createNode(void *data){
	AVLTree_Node_t *node = malloc(sizeof(AVLTree_Node_t));
	node->balanceFactor = 0;
	node->data = data;
	return node;
}

static int _abs(int num){
	return (num < 0) ?  num * -1 : num;
}
