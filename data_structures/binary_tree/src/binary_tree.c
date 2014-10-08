#include <stdio.h>
#include <stdlib.h>

#include "binary_tree.h"

/**
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

void BinaryTree_free(BinaryTree_Tree_t *tree){
	BinaryTree_removeNode(tree, tree->root);
	free(tree);
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
	newNode->right = node->right;
	node->right = newNode;
	tree->size++;
}

void BinaryTree_removeNode(BinaryTree_Tree_t *tree, BinaryTree_Node_t *node){
	if(node == NULL){
		return;
	}
	else {
		BinaryTree_removeNode(tree, node->left);
		BinaryTree_removeNode(tree, node->right);
		tree->freeData(node->data);
		free(node);
		tree->size--;
	}
}

bool BinaryTree_isLeaf(const BinaryTree_Node_t *node){
	return !node->left && !node->right;
}

void BinaryTree_preOrder(
	const BinaryTree_Node_t *node, void action(void *data)
){
	if(node != NULL){
		action(node->data);
		BinaryTree_preOrder(node->left, action);
		BinaryTree_preOrder(node->right, action);
	}
}

void BinaryTree_inOrder(
	const BinaryTree_Node_t *node, void action(void *data)
){
	if(node != NULL){
		BinaryTree_inOrder(node->left, action);
		action(node->data);
		BinaryTree_inOrder(node->right, action);
	}
}

void BinaryTree_postOrder(
	const BinaryTree_Node_t *node, void action(void *data)
){
	if(node != NULL){
		BinaryTree_postOrder(node->left, action);
		BinaryTree_postOrder(node->right, action);
		action(node->data);
	}
}

static BinaryTree_Node_t *BinaryTree_createNode(void *data){
	BinaryTree_Node_t *node = malloc(sizeof(BinaryTree_Node_t));
	node->data = data;
	node->left = NULL;
	node->right = NULL;
	return node;
}
