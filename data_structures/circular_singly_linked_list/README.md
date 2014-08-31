# circular singly-linked list
An implementation of a [circular singly-linked list](http://en.wikipedia.org/wiki/Linked_list#Circular_list). The
data-structure itself, and any functions used to modify and interact with it, are defined in
`src/circular_singly_linked_list.c`, which is designed so that it can be easily compiled into any other project. Its
nodes' data is referenced by `void` pointers, and is thus generalized for any data-type. `src/test.c` contains a
main-block and function calls to test everything inside `circular_singly_linked_list.c`: some diagnostic messages will
be printed at runtime.

 * `make` : Compile the project.
 * `make run` : Run the compiled executable.
 * `make clean` : Remove any compiled files

An example of using the API defined by `circular_singly_linked_list.c` in another C file:

```c
#include <stdlib.h>
#include <string.h>

#include "circular_singly_linked_list.h"

static void freeData(void *data){
	free((char *)data);
}

int main(){
	CSLList_t *list = createCSLList(freeData);
	char *string = malloc(11);
	strcpy(string, "0xdeadbeef");
	insertCSLListHead(list, string);
	printCSLList(list, "%s\n");
	freeCSLList(list);
	return 0;
}
```
