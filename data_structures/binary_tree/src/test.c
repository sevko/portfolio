#include <stdio.h>
#include <stdlib.h>

#include "src/binary_tree.h"

void freeData(void *data){
	free((char *)data);
}

void testBinaryTree(void){
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);
}

int main(){
	testBinaryTree();
	return EXIT_SUCCESS;
}
