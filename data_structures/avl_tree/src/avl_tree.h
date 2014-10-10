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

/*
 * @brief Create an AVL tree.
 * @param freeData A function to deallocate the data stored in this tree's
 *      nodes. Will be called by `AVLTree_free()` if the tree has any nodes
 *      remaining, so, if you're absolutely certain that it will be empty when
 *      you `AVLTree_free()` it, you can pass in a `NULL` pointer here.
 * @return A pointer to the newly allocated tree.
*/
AVLTree_Tree_t *AVLTree_create(void (*freeData)(void *data));

/*
 * @brief Deallocate an AVL tree.
 * @param tree If the tree has any nodes, they will be deallocated, and their
 *      `data` members freed with `tree->freeData()`. It will be `free()`d
 *      afterwards.
*/
void AVLTree_free(AVLTree_Tree_t *tree);
void AVLTree_insert(AVLTree_Tree_t *tree, void *data);
void *AVLTree_remove(AVLTree_Tree_t *tree, void *data);
void *AVLTree_find(void *data);
