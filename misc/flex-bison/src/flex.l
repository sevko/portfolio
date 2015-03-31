/**
 * The Snazzle Flex tokenizer.
 */

%{
#include <stdio.h>

#define YY_DECL extern int yylex()
#include "bin/bison.h"
#define PRINT_TOK(msg) printf(msg ": `%.*s`\n", yyleng, yytext)
int lineNum = 0;
%}

%%

[ \t] ; // ignore whitespace
sNaZZle return SNAZZLE;
type return TYPE;
end return END;
[0-9]+\.[0-9]+ {
	yylval.fval = atof(yytext);
	return FLOAT;
}

[0-9]+ {
	yylval.ival = atoi(yytext);
	return INT;
}

[a-zA-Z0-9]+ {
	yylval.sval = strdup(yytext);
	return STRING;
}
\n+ {
	lineNum += yyleng;
	return ENDL;
}
. ;

%%
