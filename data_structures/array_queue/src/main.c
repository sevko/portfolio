#include <stdio.h>
#include "array_queue.h"
#include <tap.h>

static void _test_ArrayQueue(void){
	ArrayQueue_t *queue = ArrayQueue_create();
	int NUM_TEST_VALUES = 100;
	int numbers[NUM_TEST_VALUES];

	note("Testing peek(), size(), and empty().");
	for(int ind = 0; ind < NUM_TEST_VALUES; ind++){
		numbers[ind] = ind;
		ArrayQueue_enqueue(queue, &numbers[ind]);
		ok(
			*(int *)ArrayQueue_peek(queue) == numbers[0],
			"Head is %d.", numbers[0]
		);
		ok(ArrayQueue_size(queue) == ind + 1, "Size is %d.", ind + 1);
		ok(!ArrayQueue_empty(queue), "Queue is not empty.");
	}

	note("Testing peekIndex().");
	for(int ind = 0; ind < NUM_TEST_VALUES; ind++){
		ok(
			*(int *)ArrayQueue_peekIndex(queue, ind) == numbers[ind],
			"peekIndex(%d) returns %d.", ind, numbers[ind]
		);
	}

	note("Testing dequeue().");
	int ind = 0;
	while(!ArrayQueue_empty(queue)){
		int dequeued = *(int *)ArrayQueue_dequeue(queue);
		ok(
			dequeued == numbers[ind++], "Dequeued value %d matches expected.",
			dequeued
		);
	}

	ArrayQueue_destroy(queue);
}

int main(){
	note("Begin tests for ArrayQueue");
	_test_ArrayQueue();
	done_testing();
	return 0;
}
