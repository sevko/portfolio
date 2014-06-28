#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "src/singly_linked_list.h"

/*
 * @brief Tests the other functions defined within this source file.
*/
static void testSLList(void);

void freeData(void *data){
	free((char *)data);
}

static void testSLList(void){
	char *s0 = malloc(20);
	strcpy(s0, "0000000000");
	char *s1 = malloc(20);
	strcpy(s1, "1111111111");
	char *s2 = malloc(20);
	strcpy(s2, "2222222222");
	char *s3 = malloc(20);
	strcpy(s3, "3333333333");
	char *s4 = malloc(20);
	strcpy(s4, "4444444444");
	char *s5 = malloc(20);
	strcpy(s5, "5555555555");
	char *s6 = malloc(20);
	strcpy(s6, "6666666666");
	char *s7 = malloc(20);
	strcpy(s7, "7777777777");

	SLList_t *list = createSLList(freeData);
	insertSLListHead(list, s0);
	insertSLListHead(list, s1);
	insertSLListHead(list, s2);
	insertSLListHead(list, s3);
	insertSLListHead(list, s4);
	insertSLListHead(list, s5);
	insertSLListHead(list, s6);
	insertSLListHead(list, s7);
	freeSLList(list);
}

int main(){
	testSLList();
	return EXIT_SUCCESS;
}
