%{
#include <stdio.h>
#include <error.h>

#define PRINT_TOK(msg) printf(msg ": `%.*s`\n", yyleng, yytext)
%}

%%

[ \t\n] ; // ignore whitespace
[0-9]+\.[0-9]+ PRINT_TOK("floating-point number");
[0-9]+ PRINT_TOK("integer");
[a-zA-Z0-9]+ PRINT_TOK("string");

%%

int main(){
	const char *filePath = "corpus.txt";
	FILE *file = fopen(filePath, "r");
	if(file == NULL){
		fprintf(
			stderr, "Couldn't open `%s` because of `%s`.\n",
			filePath, strerror(errno)
		);
		return 1;
	}

	yyin = file;
	yylex();
	return 0;
}
