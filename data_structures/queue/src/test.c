/*
	@brief Test the queue implemented in `queue.c`.
*/

#include <stdio.h>
#include <stdlib.h>

#include "src/queue.h"

/*
 * @brief Deallocates data stored in the test ::Queue_t's nodes.
 *
 * @param data Assumed to be a `char *`.
*/
static void freeData(void *data);

/*
	@brief Test various ::Queue_t functions.
*/
static void testQueue(void);

static void freeData(void *data){
	free((char *)data);
}

static void testQueue(void){
	int numStrings = 9;
	char *strings[numStrings];
	int ind;
	for(ind = 0; ind < numStrings; ind++){
		strings[ind] = malloc(2);
		sprintf(strings[ind], "%d", ind);
	}

	Queue_t *queue = createQueue(freeData);
	enqueue(queue, strings[0]);
	enqueue(queue, strings[1]);
	enqueue(queue, strings[2]);
	enqueue(queue, strings[3]);
	enqueue(queue, strings[4]);
	enqueue(queue, strings[5]);
	enqueue(queue, strings[6]);
	enqueue(queue, strings[7]);
	enqueue(queue, strings[8]);
	freeQueue(queue);
}

int main(void){
	testQueue();
	return EXIT_SUCCESS;
}
