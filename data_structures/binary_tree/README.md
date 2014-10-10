# binary tree
An implementation of a [binary tree](http://en.wikipedia.org/wiki/Binary_tree). The
data-structure itself, and any functions used to modify and interact with it, are defined in
`src/binary_tree.c`, which is designed so that it can be easily compiled into any other project. Its methods
rely on `void *` pointers, and are thus generalized for any data-type. `src/test.c` contains
[TAP](http://testanything.org/) unit tests (via [libtap](https://github.com/zorgnax/libtap)), and is the main entry
point for the project's generated executable.

 * `make` : Compile the project.
 * `make run` : Run the compiled executable, which contains TAP unit tests. Pipe them through something like
   `tap-spec` for human-readable output.
 * `make clean` : Remove any compiled files.

An example of using some of the API defined by `binary_tree.c` in another C file:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binary_tree.h"

static void freeData(void *data){
	free((char *)data);
}

static void printData(void *data){
	printf("%s\n", (char *)data);
}

int main(){
	BinaryTree_Tree_t *tree = BinaryTree_create(freeData);
	char *data1 = malloc(7),
		*data2 = malloc(7);
	strcpy(data1, "0xdead");
	strcpy(data2, "0xbeef");

	BinaryTree_insertRoot(tree, data1);
	BinaryTree_insertLeft(tree, tree->root, data2);
	BinaryTree_travBreadth(tree->root, printData);
	BinaryTree_free(tree);
}
```
