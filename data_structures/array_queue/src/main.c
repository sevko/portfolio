#include <stdio.h>
#include "array_queue.h"

int main(){
	int numbers[] = {1, 2, 3, 4, 5, 6, 7};
	ArrayQueue_t *queue = ArrayQueue_create();
	for(int ind = 0; ind < (signed)(sizeof(numbers) / sizeof(numbers[0])); ind++){
		ArrayQueue_enqueue(queue, &numbers[ind]);
	}
	while(!ArrayQueue_empty(queue)){
		printf("Dequeued: %d\n", *(int *)ArrayQueue_dequeue(queue));
	}
	ArrayQueue_destroy(queue);
	return 0;
}
