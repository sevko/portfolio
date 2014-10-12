/*
 * @brief Unit tests for the `avl_tree` module.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tap.h>

#include "avl_tree.h"

// See `_saveTraversedData()`.
char **g_testData;
int g_testDataInd;

/**
 * @brief Passed to `BinaryTree_create()`.
 * @param data The data item (cast to a `char *`, which is the data type used
 *      by these unit tests) to deallocate.
*/
static void _freeData(void *data){
	free((char *)data);
}

/*
 * @brief Compare two data items; used by the AVL tree.
 * @param data1 The first data item; assumed to be a `char *`.
 * @param data2 The second data item; assumed to be a `char *`.
 * @return A number less than 0 if `data1` is less than `data2`; 0 if they're
 *      equal; a number greater than 0 otherwise.
*/
static int _compareData(const void *data1, const void *data2){
	return strcmp((const char *)data1, (const char *)data2);
}

/**
 * @brief Create a number of unique strings, for testing.
 * @param numData The number of test strings to create.
 * @return `numData` strings containing single characters; suitable for testing
 *      use as data items. Free them with `test_freeDataItems()`.
*/
static char **_test_createDataItems(int numData){
	char **data = malloc(numData * sizeof(char *));
	for(int item = 0; item < numData; item++){
		data[item] = malloc(2);
		data[item][0] = item + (int)'a';
		data[item][1] = 0;
	}
	return data;
}

/*
 * @brief Test `AVLTree_create()`.
*/
static void _test_create(void){
	note("Test AVLTree_create().");
	AVLTree_Tree_t *tree = AVLTree_create(_freeData, _compareData);

	ok(tree->size == 0, "Tree size set to 0.");
	ok(tree->root == NULL, "Tree root is null.");
	BinaryTree_free(tree);
}

/*
 * Temporary, for testing.
*/
static void _printData(void *data){
	printf("%s, ", (char *)data);
}

/*
 * @brief Passed to various traversal methods to verify that data items are
 *      being accessed in the right order.
 * @param data Any one of a given tree's nodes' data. Assumed to `char *`.
*/
static void _saveTraversedData(void *data){
	g_testData[g_testDataInd++] = data;
}

/*
 * @brief Test `AVLTree_insert()`.
*/
static void _test_insert(void){
	note("Test AVLTree_insert().");
	AVLTree_Tree_t *tree = AVLTree_create(_freeData, _compareData);
	int numData = 7;

	g_testDataInd = 0;
	g_testData = malloc(numData * sizeof(char *));

	char **testData = _test_createDataItems(numData);
	char **expected = (char *[]){"d", "b", "f", "a", "c", "e", "g"};

	AVLTree_insert(tree, testData[3]);
	AVLTree_insert(tree, testData[1]);
	AVLTree_insert(tree, testData[5]);
	AVLTree_insert(tree, testData[0]);
	AVLTree_insert(tree, testData[2]);
	AVLTree_insert(tree, testData[4]);
	AVLTree_insert(tree, testData[6]);
	BinaryTree_travBreadth(tree->root, _saveTraversedData);

	for(int ind = 0; ind < numData; ind++){
		is(
			g_testData[ind], expected[ind],
			"Inserted data item %d matches", ind
		);
	}

	free(g_testData);
	free(testData);
	BinaryTree_free(tree);
}

int main(){
	note("Begin unit tests.");
	_test_create();
	_test_insert();
	done_testing();
	return EXIT_SUCCESS;
}
