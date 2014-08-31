# singly-linked list
An implementation of a [singly-linked list](http://en.wikipedia.org/wiki/Linked_list#Singly_linked_list). The
data-structure itself, and any functions used to modify and interact with it, are defined in
`src/singly_linked_list.c`, which is designed so that it can be easily compiled into any other project. Its methods
rely on `void *` pointers, and are thus generalized for any data-type. `src/test.c` contains a main-block and function
calls to test everything inside `singly_linked_list.c`: some diagnostic messages will be printed at runtime.

 * `make` : Compile the project.
 * `make run` : Run the compiled executable.
 * `make clean` : Remove any compiled files

An example of using the API defined by `singly_linked_list.c` in another C file:

```c
#include <stdlib.h>
#include <string.h>

#include "singly_linked_list.h"

static void freeData(void *data){
	free((char *)data);
}

int main(){
	SLList_t *list = createSLList(freeData);
	char *string = malloc(11);
	strcpy(string, "0xdeadbeef");
	insertSLListHead(list, string);
	freeSLList(list);
	return 0;
}
```
