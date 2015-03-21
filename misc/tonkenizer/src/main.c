#include <stdio.h>
#include <sys/time.h>
#include <locale.h>
#include <stdlib.h>

#include "src/tokenizer.h"

char *readFile(const char *filePath, int *fileLength){
	FILE *file = fopen(filePath, "r");
	fseek(file, 0, SEEK_END);
	long length = ftell(file);
	rewind(file);
	char *contents = malloc(length);
	if(fread(contents, 1, length, file) != (unsigned)length){
		fprintf(stderr, "Could not read all of file: %s\n", filePath);
		exit(1);
	}
	fclose(file);
	*fileLength = length;
	return contents;
}

/**
 * Returns the current time in microseconds.
 */
long getMicrotime(){
	struct timeval currentTime;
	gettimeofday(&currentTime, NULL);
	return currentTime.tv_sec * (int)1e6 + currentTime.tv_usec;
}

int main(){
	int fileLength;
	char *corpus = readFile("corpus.txt", &fileLength);

	initTokenizer();
	unsigned long startTime = getMicrotime();
	int numTokens;
	Token_t *tokens = getTokens(corpus, fileLength, &numTokens);

	setlocale(LC_NUMERIC, "");
	printf(
		"Time taken: %ldus\n"
		"File length: %'d chars\n"
		"Number tokens: %'d\n",
		getMicrotime() - startTime, fileLength, numTokens);

	free(corpus);
	free(tokens);
	deinitTokenizer();
	return 0;
}
