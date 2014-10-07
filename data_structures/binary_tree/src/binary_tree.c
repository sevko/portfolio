#include <stdio.h>
#include <stdlib.h>

#include "binary_tree.h"

/*
 * @brief Allocate memory for a new node.
 * @param data The `data` member of the new node.
 * @return A pointer to the new node.
*/
static BinaryTree_Node_t *BinaryTree_createNode(void *data);

BinaryTree_Tree_t *BinaryTree_create(void (*freeData)(void *data)){
	BinaryTree_Tree_t *tree = malloc(sizeof(BinaryTree_Tree_t));
	tree->size = 0;
	tree->freeData = freeData;
	tree->root = NULL;
	return tree;
}

void BinaryTree_insertRoot(BinaryTree_Tree_t *tree, void *data){
	BinaryTree_Node_t *newNode = BinaryTree_createNode(data);
	tree->root = newNode;
	tree->size++;
}

void BinaryTree_insertLeft(
	BinaryTree_Tree_t *tree, BinaryTree_Node_t *node, void *data
){
	BinaryTree_Node_t *newNode = BinaryTree_createNode(data);
	newNode->left = node->left;
	node->left = newNode;
	tree->size++;
}

void BinaryTree_insertRight(
	BinaryTree_Tree_t *tree, BinaryTree_Node_t *node, void *data
){
	BinaryTree_Node_t *newNode = BinaryTree_createNode(data);
	newNode->left = node->left;
	node->left = newNode;
	tree->size++;
}

static BinaryTree_Node_t *BinaryTree_createNode(void *data){
	BinaryTree_Node_t *node = malloc(sizeof(BinaryTree_Node_t));
	node->data = data;
	node->left = NULL;
	node->right = NULL;
	return node;
}
