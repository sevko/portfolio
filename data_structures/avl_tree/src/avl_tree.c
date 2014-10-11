#include <stdio.h>
#include <stdlib.h>

#include "avl_tree.h"

AVLTree_Tree_t *AVLTree_create(
	void (*freeData)(void *data),
	int (*compareData)(const void *data1, const void *data2)
){
	AVLTree_Tree_t *tree = BinaryTree_create(freeData);
	tree->compareData = compareData;
	return tree;
}

void AVLTree_insert(AVLTree_Tree_t *tree, BinaryTree_Node_t *node, void *data){
	if(tree->compareData(node->data, data) < 0){
		if(node->right){
			AVLTree_insert(tree, node->right, data);
		}
		else {
			BinaryTree_insertRight(tree, node, data);
		}
	}
	else {
		if(node->left){
			AVLTree_insert(tree, node->left, data);
		}
		else {
			BinaryTree_insertLeft(tree, node, data);
		}
	}
}
