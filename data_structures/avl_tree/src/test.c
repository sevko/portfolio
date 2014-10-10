/*
 * @brief Unit tests for the `avl_tree` module.
*/

#include <stdio.h>
#include <stdlib.h>
#include <tap.h>

#include "avl_tree.h"

/**
 * @brief Passed to `BinaryTree_create()`.
 * @param data The data item (cast to a `char *`, which is the data type used
 *      by these unit tests) to deallocate.
*/
void freeData(void *data){
	free((char *)data);
}

/*
 * @brief Test `AVLTree_create()`.
*/
static void _test_create(void){
	note("Test AVLTree_create().");
	AVLTree_Tree_t *tree = AVLTree_create(freeData);

	ok(tree->size == 0, "Tree size set to 0.");
	ok(tree->root == NULL, "Tree root is null.");
	BinaryTree_free(tree);
}

int main(){
	note("Begin unit tests.");
	_test_create();
	done_testing();
	return EXIT_SUCCESS;
}
