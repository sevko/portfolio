#include <stdio.h>
#include <stdlib.h>

#include "binary_tree.h"

BinaryTree_Tree_t *BinaryTree_create(void (*freeData)(void *data)){
	BinaryTree_Tree_t *tree = malloc(sizeof(BinaryTree_Tree_t));
	tree->size = 0;
	tree->freeData = freeData;
	tree->root = NULL;
	return tree;
}
