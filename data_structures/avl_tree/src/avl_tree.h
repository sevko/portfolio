/**
 * @brief An implementation of an AVL tree.
 */

#pragma once

#include "binary_tree.h"

// One node of the AVL tree.
typedef struct {
	void *data; // The data contained inside this node.
	int balanceFactor; // The balance factor of this node.
} AVL_Node_t;

// An AVL tree.
typedef BinaryTree_Tree_t AVL_Tree_t;

AVL_Tree_t *AVL_create(void (*freeData)(void *data));
void AVL_free(AVL_Tree_t *tree);
void AVL_insert(AVL_Tree_t *tree, void *data);
void *AVL_remove(AVL_Tree_t *tree, void *data);
void *AVL_find(void *data);
