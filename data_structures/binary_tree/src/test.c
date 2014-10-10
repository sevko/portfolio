#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tap.h>

#include "src/binary_tree.h"

// Used by `test_traversals()`.
char **g_traversalTest;
int g_traversalTestInd;

/**
 * @brief Passed to `BinaryTree_create()`.
 * @param data The data item (cast to a `char *`, which is the data type used
 *      by these unit tests) to deallocate.
*/
void freeData(void *data){
	free((char *)data);
}

/*
 * @brief Used by `test_traversals()`.
 * @param data A data pointer to cast to `char *` and print.
*/
void test_traversalResult(void *data){
	g_traversalTest[g_traversalTestInd++] = (char *)data;
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
	ok(tree->size == 0, "Tree size is 0.");
	tree->root = NULL;
	BinaryTree_free(tree);
	free(data);
}

static void test_isLeaf(void){
	note("Test BinaryTree_isLeaf().");
	char **data = test_createDataItems(2);
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);
	BinaryTree_insertRoot(tree, data[0]);
	BinaryTree_insertLeft(tree, tree->root, data[1]);
	ok(!BinaryTree_isLeaf(tree->root), "Identifies non-leaves.");
	ok(BinaryTree_isLeaf(tree->root->left), "Identifies leaves.");
	BinaryTree_free(tree);
	free(data);
}

static void test_traversals(void){
	int numData = 7;
	g_traversalTest = malloc(numData * sizeof(char *));
	char **data = test_createDataItems(numData);
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);

	BinaryTree_insertRoot(tree, data[0]);
	BinaryTree_insertRight(tree, tree->root, data[1]);
	BinaryTree_insertLeft(tree, tree->root->right, data[2]);

	BinaryTree_insertLeft(tree, tree->root, data[3]);
	BinaryTree_insertLeft(tree, tree->root->left, data[4]);
	BinaryTree_insertRight(tree, tree->root->left, data[5]);
	BinaryTree_insertLeft(tree, tree->root->left->right, data[6]);

	note("Test BinaryTree_travPreOrder().");
	BinaryTree_travPreOrder(tree->root, test_traversalResult);

	char **expected = (char *[]){"a", "d", "e", "f", "g", "b", "c"};
	for(int ind = 0; ind < g_traversalTestInd; ind++){
		is(g_traversalTest[ind], expected[ind], "Data %d matches.", ind + 1);
	}

	note("Test BinaryTree_travInOrder().");
	g_traversalTestInd = 0;
	BinaryTree_travInOrder(tree->root, test_traversalResult);
	expected = (char *[]){"e", "d", "g", "f", "a", "c", "b"};
	for(int ind = 0; ind < g_traversalTestInd; ind++){
		is(g_traversalTest[ind], expected[ind], "Data %d matches.", ind + 1);
	}

	note("Test BinaryTree_travPostOrder().");
	g_traversalTestInd = 0;
	BinaryTree_travPostOrder(tree->root, test_traversalResult);
	expected = (char *[]){"e", "g", "f", "d", "c", "b", "a"};
	for(int ind = 0; ind < g_traversalTestInd; ind++){
		is(g_traversalTest[ind], expected[ind], "Data %d matches.", ind + 1);
	}

	BinaryTree_free(tree);
	free(g_traversalTest);
	free(data);
}

static void test_travBreadth(void){
	int numData = 7;
	g_traversalTest = malloc(numData * sizeof(char *));
	char **data = test_createDataItems(numData);
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);

	BinaryTree_insertRoot(tree, data[0]);
	BinaryTree_insertRight(tree, tree->root, data[1]);
	BinaryTree_insertLeft(tree, tree->root->right, data[2]);

	BinaryTree_insertLeft(tree, tree->root, data[3]);
	BinaryTree_insertLeft(tree, tree->root->left, data[4]);
	BinaryTree_insertRight(tree, tree->root->left, data[5]);
	BinaryTree_insertLeft(tree, tree->root->left->right, data[6]);

	g_traversalTestInd = 0;
	BinaryTree_travBreadth(tree->root, test_traversalResult);
	char **expected = (char *[]){"a", "d", "b", "e", "f", "c", "g"};
	for(int ind = 0; ind < g_traversalTestInd; ind++){
		is(g_traversalTest[ind], expected[ind], "Data %d matches.", ind + 1);
	}

	BinaryTree_free(tree);
	free(g_traversalTest);
	free(data);
}

int main(){
	note("Begin unit tests.");
	test_create();
	test_insertRoot();
	test_insertLeft();
	test_removeNode();
	test_isLeaf();
	test_traversals();
	test_travBreadth();
	done_testing();
	return EXIT_SUCCESS;
}
