#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tap.h>

#include "src/binary_tree.h"

/**
 * @brief Passed to `BinaryTree_create()`.
 * @param data The data item (cast to a `char *`, which is the data type used
 *      by these unit tests) to deallocate.
*/
void freeData(void *data){
	free((char *)data);
}

/**
 * @brief Create a number of unique strings, for testing.
 * @param numData The number of test strings to create.
 * @return `numData` strings containing single characters; suitable for testing
 *      use as data items. Free them with `test_freeDataItems()`.
*/
static char **test_createDataItems(int numData){
	char **data = malloc(numData * sizeof(char *));
	for(int item = 0; item < numData; item++){
		data[item] = malloc(2);
		data[item][0] = item + (int)'a';
		data[item][1] = 0;
	}
	return data;
}

/**
 * @brief Free the strings allocated by `test_createDataItems()`.
 * @param data As returned yb `test_createDataItems()`.
 * @param numData The number of `char *` inside `data` (ie, the `numData`
 *      argument to `test_createDataItems()`).
*/
static void test_freeDataItems(char **data, int numData){
	for(int ind = 0; ind < numData; ind++){
		free(data[ind]);
	}
	free(data);
}

static void test_create(void){
	note("Test BinaryTree_create().");
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);

	ok(tree->size == 0, "Tree size set to 0.");
	ok(tree->root == NULL, "Tree root is null.");
	BinaryTree_free(tree);
}

static void test_insertRoot(void){
	note("Test BinaryTree_insertRoot().");
	char *data = malloc(2);
	strcpy(data, "a");
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);

	BinaryTree_insertRoot(tree, data);
	ok(tree->size == 1, "Tree size is 1.");
	is(tree->root->data, data, "Root node contains correct data.");
	BinaryTree_free(tree);
}

static void test_insertLeft(void){
	note("Test BinaryTree_insertLeft().");
	char **data = test_createDataItems(3);
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);

	BinaryTree_insertRoot(tree, data[0]);
	BinaryTree_insertLeft(tree, tree->root, data[1]);
	is(tree->root->left->data, data[1], "Leaf node inserted correctly.");

	BinaryTree_insertLeft(tree, tree->root, data[2]);
	is(tree->root->left->data, data[2], "Non-leaf node inserted correctly.");
	is(tree->root->left->left->data, data[1], "Existing nodes preserved.");
	BinaryTree_free(tree);
	free(data);
}

static void test_removeNode(void){
	note("Test BinaryTree_removeNode().");
	char **data = test_createDataItems(4);
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);

	BinaryTree_insertRoot(tree, data[0]);
	BinaryTree_insertLeft(tree, tree->root, data[1]);
	BinaryTree_insertLeft(tree, tree->root->left, data[2]);
	BinaryTree_insertRight(tree, tree->root->left, data[3]);
	BinaryTree_removeNode(tree, tree->root);
	tree->root = NULL;
	BinaryTree_free(tree);
	free(data);
}

int main(){
	note("Begin unit tests.");
	test_create();
	test_insertRoot();
	test_insertLeft();
	test_removeNode();
	done_testing();
	return EXIT_SUCCESS;
}
