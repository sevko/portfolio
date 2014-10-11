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
