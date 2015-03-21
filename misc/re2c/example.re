#define _GNU_SOURCE

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
	TOK_KEYWORD, TOK_IDENTIFIER, TOK_WORD, TOK_SYMBOL, TOK_WHITESPACE,
	TOK_NONE
} TokenType_t;
typedef struct {
	const char *body;
	int length;
	TokenType_t type;
} Token_t;

char *tokenToString(Token_t *token){
	const char *tokenTypeStr;

	#define CASE(tokType) \
		case TOK_##tokType:\
			tokenTypeStr = #tokType;\
			break;

	switch(token->type){
		CASE(KEYWORD)
		CASE(IDENTIFIER)
		CASE(WORD)
		CASE(SYMBOL)
		CASE(WHITESPACE)
		CASE(NONE)

		default:
			tokenTypeStr = "UNRECOGNIZED";
			break;
	}
	char *str;
	int bytesWritten = asprintf(
		&str, "%s: `%.*s`", tokenTypeStr, token->length, token->body
	);
	return bytesWritten == -1 ? NULL : str;
}

Token_t *tokenize(const char *src, int *numTotalTokens){
	#define YYCTYPE char
	#define YYCURSOR src
	#define YYMARKER temp

	int tokenBufLength = 100;
	Token_t *tokens = malloc(tokenBufLength * sizeof(Token_t));
	int numTokens = 0;

	while(true){
		const char *start = YYCURSOR;
		TokenType_t type;
		Token_t *token = &(tokens[numTokens++]);
		/*!re2c
		re2c:yyfill:enable = 0;

		"int"|"void" {
			type = TOK_KEYWORD;
			goto CREATE_TOKEN;
		}

		ANYLETTER = [a-zA-Z];
		ANYLETTER+ {
			type = TOK_IDENTIFIER;
			goto CREATE_TOKEN;
		}

		";" {
			type = TOK_SYMBOL;
			goto CREATE_TOKEN;
		}

		[ \r\t]+ {
			type = TOK_WHITESPACE;
			goto CREATE_TOKEN;
		}

		"\x00" {
			*numTotalTokens = numTokens - 1;
			return tokens;
		}

		[^] {
			puts("Invalid token");
			continue;
		}
		*/

		CREATE_TOKEN:
		token->body = start;
		token->length = YYCURSOR - start;
		token->type = type;

		if(numTokens == tokenBufLength){
			tokenBufLength *= 2;
			tokens = realloc(tokens, tokenBufLength * sizeof(Token_t));
		}
	}
}

int main(void){
	int numTokens;
	Token_t *tokens = tokenize("int x; int y; void", &numTokens);
	for(int ind = 0; ind < numTokens; ind++){
		char *str = tokenToString(&tokens[ind]);
		puts(str);
		free(str);
	}
	free(tokens);
	return 0;
}
