/**
 * A toy tokenizer built with re2c.
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// The various token types recognized by the parser.
typedef enum {
	TOK_KEYWORD, TOK_IDENTIFIER, TOK_WORD, TOK_SYMBOL, TOK_WHITESPACE,
	TOK_NONE
} TokenType_t;

// A token.
typedef struct {
	const char *body; // The contents of the token.
	int length; // The number of bytes in `body`.
	TokenType_t type;
} Token_t;

/**
 * Return a human-readable, string representation of a token (must be
 * `free()'d` after use).
 */
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

/**
 * Attempt extracting a token from `src`. If one is found, the number of bytes
 * read is returned and the token is stored in `*token`; a 0 is returned on
 * EOF; a -1 is returned when an un-tokenizable character was encountered.
 */
int getToken(const char *src, Token_t *token){
	#define YYCTYPE char
	#define YYCURSOR src
	#define YYMARKER temp
	TokenType_t type;
	const char *start = YYCURSOR;

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
		return 0;
	}

	[^] {
		puts("Invalid token");
	}
	*/

	CREATE_TOKEN:
	token->body = start;
	token->length = YYCURSOR - start;
	token->type = type;
	return token->length;
}

/**
 * Tokenize a `string` using `getToken()`, and return an array of all the
 * resultant `Token_t`s. The number of tokens will be stored in
 * `*numTotalTokens`. Note that `src` is expected to be null-terminated.
 */
Token_t *tokenize(const char *src, int *numTotalTokens){
	int tokenBufLength = 100;
	Token_t *tokens = malloc(tokenBufLength * sizeof(Token_t));
	int numTokens = 0;

	while(true){
		Token_t *token = &(tokens[numTokens++]);
		int length = getToken(src, token);
		switch(length){
			case -1:
				free(tokens);
				*numTotalTokens = 0;
				return NULL;
				break;

			case 0:
				*numTotalTokens = numTokens - 1;
				return tokens;
				break;

			default:
				src += length;
		}

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
