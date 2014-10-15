# AVL tree
An implementation of an [AVL](http://en.wikipedia.org/wiki/AVL_tree) (**Georgy Adelson-Velsky and Landis**) tree, or a
self-balancing binary search tree. The data-structure itself, and any functions used to modify and interact with it,
are defined in `src/avl_tree.c`, which is designed so that it can be easily compiled into any other project. Its
methods rely on `void *` pointers, and are thus generalized for any data-type. `src/test.c` contains
[TAP](http://testanything.org/) unit tests (via [libtap](https://github.com/zorgnax/libtap)), and is the main entry
point for the project's generated executable.

 * `make` : Compile the project.
 * `make run` : Run the compiled executable, which contains TAP unit tests. Pipe the output through something like
   `tap-spec` for human-readable output.
 * `make clean` : Remove any compiled files.

An example of using some of the API defined by `avl_tree.c` in another C file:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "avl_tree.h"

static void freeData(void *data){
	free(((AVLTree_Node_t *)data)->data);
	free(data);
}

static int compareData(const void *data1, const void *data2){
	return strcmp((const char *)data1, (const char *)data2);
}

static void printData(void *data){
	printf("%s\n", (char *)((AVLTree_Node_t *)data)->data);
}

int main(){
	char *data1 = malloc(7),
		*data2 = malloc(7);
	strcpy(data1, "0xdead");
	strcpy(data2, "0xbeef");

	AVLTree_Tree_t *tree = AVLTree_create(freeData, compareData);
	AVLTree_insert(tree, data1);
	AVLTree_insert(tree, data2);
	BinaryTree_travBreadth(tree->root, printData);
	BinaryTree_free(tree);
}
```
