#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tap.h>

#include "src/binary_tree.h"

void freeData(void *data){
	free((char *)data);
}

static void test_create(void){
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);
	plan(NO_PLAN);
	ok(tree->size == 0, "Tree size set to 0.");
	ok(tree->root == NULL, "Tree root is null.");
}

static void test_insertRoot(void){
	char *data = malloc(2);
	strcpy(data, "a");
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);
	BinaryTree_insertRoot(tree, data);
	ok(tree->size == 1, "Tree size is 1.");
	is(tree->root->data, data, "Root node contains correct data.");
}

static void test_insertLeft(void){
	const int numData = 3;
	char *data[numData];
	for(int item = 0; item < numData; item++){
		data[item] = malloc(2);
		data[item][0] = item + (int)'a';
	}

	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);
	BinaryTree_insertRoot(tree, data[0]);

	BinaryTree_insertLeft(tree, tree->root, data[1]);
	is(tree->root->left->data, data[1], "Leaf node inserted correctly.");

	BinaryTree_insertLeft(tree, tree->root, data[2]);
	is(tree->root->left->data, data[2], "Non-leaf node inserted correctly.");
	is(tree->root->left->left->data, data[1], "Existing nodes preserved.");
}

int main(){
	test_create();
	test_insertRoot();
	test_insertLeft();
	done_testing();
	return EXIT_SUCCESS;
}
