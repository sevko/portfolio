%{
#include <stdio.h>
%}

%%

[ \t\n] ;
[0-9]+\.[0-9]+ puts("Found a floating-point number:");
[0-9]+ puts("Found an integer:");
[a-zA-Z0-9]+ puts("Found a string: ");

%%

int main(){
	yylex();
	return 0;
}
