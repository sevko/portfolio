/**
 * A simple `cat`-like utility.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(int argc, char **argv){
	if(argc != 2){
		fputs("Incorrect number of arguments given. Expecting one file-path.\n", stderr);
		return 1;
	}

	const char *filepath = argv[1];
	FILE *file = fopen(filepath, "r");
	if(file == NULL){
		fprintf(stderr, "Failed to open `%s` with `%s`.\n", filepath, strerror(errno));
		return 1;
	}

	fseek(file, 0, SEEK_END);
	int fileLength = ftell(file);
	rewind(file);

	char *fileBuffer = malloc(fileLength);
	int bytesRead = fread(fileBuffer, 1, fileLength, file);
	if(bytesRead != fileLength){
		fprintf(stderr, "Error: able to read only %d bytes of a %d byte file.\n", bytesRead, fileLength);
		return 1;
	}
	fclose(file);
	fwrite(fileBuffer, fileLength, 1, stdout);
	free(fileBuffer);
	return EXIT_SUCCESS;
}
