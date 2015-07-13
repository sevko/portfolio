#include <stdio.h>
#include <stdlib.h>

#include "src/json_parser.h"

char *readFile(char *filepath, int *numBytesRead, bool nullTerminate){
	FILE *file = fopen(filepath, "r");
	fseek(file, 0, SEEK_END);
	int fileLength = ftell(file);
	rewind(file);

	char *fileBuffer = malloc(nullTerminate ? fileLength + 1 : fileLength);
	if(fread(fileBuffer, 1, fileLength, file) != (unsigned int)fileLength){
		fputs("Failed to read file.\n", stderr);
		exit(1);
	}
	if(nullTerminate){
		fileBuffer[fileLength] = '\0';
	}
	fclose(file);
	if(numBytesRead != NULL){
		*numBytesRead = fileLength;
	}
	return fileBuffer;
}

int main(){
	bool parseFailed;
	char *parseErrMsg;
	char *src = "{\"abc\": \"foo\"}";
	JsonVal_t parsed = parse(src, &parseFailed, &parseErrMsg);
	if(parseFailed){
		puts("Error:");
		fputs(parseErrMsg, stderr);
		free(parseErrMsg);
		return EXIT_FAILURE;
	}
	JsonVal_print(&parsed);
	JsonVal_free(&parsed);
	return EXIT_SUCCESS;
}
