/**
 * @brief An implementation of an AVL tree.
 */

#pragma once

#include "binary_tree.h"

// One node of the AVL tree.
typedef struct {
	void *data; // The data contained inside this node.
	int balanceFactor; // The balance factor of this node.
} AVLTree_Node_t;

// An AVL tree.
typedef BinaryTree_Tree_t AVLTree_Tree_t;

AVLTree_Tree_t *AVLTree_create(void (*freeData)(void *data));
void AVLTree_free(AVLTree_Tree_t *tree);
void AVLTree_insert(AVLTree_Tree_t *tree, void *data);
void *AVLTree_remove(AVLTree_Tree_t *tree, void *data);
void *AVLTree_find(void *data);
