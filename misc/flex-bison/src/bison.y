%{
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <error.h>

#define PRINTFERR(...) fprintf(stderr, __VA_ARGS__)

extern int lineNum;
extern int yylex();
extern int yyparser();
extern FILE *yyin;
void yyerror(const char *err);
%}

%union {
	int ival;
	float fval;
	char *sval;
}

%token <ival> INT
%token <fval> FLOAT
%token <sval> STRING
%token SNAZZLE TYPE
%token END ENDL

%%

snazzle:
	header template body_section footer {puts("Finished with file.");}
	;

header:
	SNAZZLE FLOAT ENDL {printf("Reading a snazzle file version: %f\n", $2);}
	;

template:
	template typeline
	| typeline
	;

typeline:
	TYPE STRING ENDL {printf("%d: New type: %s\n", lineNum, $2);}
	;

body_section:
	body_section body_line
	| body_line
	;

body_line:
	INT INT INT INT STRING ENDL {
		printf("New snazzle: %d, %d, %d, %d, %s\n", $1, $2, $3, $4, $5);
	}

footer:
	END ENDL;

%%

int main(){
	const char *filePath = "corpus.txt";
	FILE *file = fopen(filePath, "r");
	if(file == NULL){
		char *buf = malloc(1000);
		PRINTFERR(
			"Couldn't open file `%s` with `%s`.\n", filePath, strerror(errno)
		);
		return -1;
	}
	yyin = file;
	do {
		yyparse();
	} while (!feof(yyin));
	fclose(file);
}

void yyerror(const char *err){
	PRINTFERR("Err: line %d: %s\n", lineNum + 1, err);
	exit(1);
}
