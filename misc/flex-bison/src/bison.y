%{
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <error.h>

#define PRINTFERR(...) fprintf(stderr, __VA_ARGS__)

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
%token END

%%

snazzle:
	header template body_section footer {puts("Finished with file.");}
	;

header:
	SNAZZLE FLOAT {printf("Reading a snazzle file version: %f\n", $2);}
	;

template:
	typelines
	;

typelines:
	typelines typeline
	| typeline
	;

typeline:
	TYPE STRING {printf("New type: %s\n", $2);}
	;

body_section:
	body_lines
	;

body_lines:
	body_lines body_line
	| body_line
	;

body_line:
	INT INT INT INT STRING {printf("New snazzle: %d, %d, %d, %d, %s\n", $1, $2, $3, $4, $5);}

footer:
	END;

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
	fprintf(stderr, "Error: %s\n", err);
	exit(1);
}
