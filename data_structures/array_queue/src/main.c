#include <stdio.h>
#include "array_queue.h"

int main(){
	const int NUM_NUMBERS = 9;
	int numbers[NUM_NUMBERS];
	ArrayQueue_t *queue = ArrayQueue_create();
	for(int ind = 0; ind < NUM_NUMBERS; ind++){
		numbers[ind] = ind;
		ArrayQueue_enqueue(queue, &numbers[ind]);
	}
	while(!ArrayQueue_empty(queue)){
		printf("Dequeued: %d\n", *(int *)ArrayQueue_dequeue(queue));
	}
	ArrayQueue_destroy(queue);
	return 0;
}
