#include <stdio.h>
#include <stdlib.h>

#include "src/doubly_linked_list.h"

void freeData(void *data){
	free((char *)data);
}

int main(){
	DLList_t *list = createDLList(freeData);
	freeDLList(list);
	return EXIT_SUCCESS;
}
