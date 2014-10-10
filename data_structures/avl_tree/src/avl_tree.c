#include <stdio.h>
#include <stdlib.h>

#include "avl_tree.h"
// #include "binary_tree.h"

AVLTree_Tree_t *AVLTree_create(void (*freeData)(void *data)){
	return BinaryTree_create(freeData);
}

void AVLTree_free(AVLTree_Tree_t *tree){
	BinaryTree_free(tree);
}
