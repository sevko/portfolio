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
	TOK_NONE, TOK_END
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
 * Attempt reading a token from `*src`. If one is found, the `TokenType_t` of
 * the token is returned, and `*src` is set to the first byte right after the
 * last matching byte of the token.
 */
TokenType_t readToken(const char **src){
	const char *temp;
	/**
	 * Silence the `unused variable` warning if `re2c` ends up not using
	 * `temp`, which it will for simple parsers.
	 */
	(void)temp;

	/*!re2c
	re2c:yyfill:enable = 0;
	re2c:define:YYCURSOR = *src;
	re2c:define:YYCTYPE = char;
	re2c:define:YYMARKER = temp;

	"int"|"void" {return TOK_KEYWORD;}
	[a-zA-Z]+ {return TOK_IDENTIFIER;}
	";" {return TOK_SYMBOL;}
	[ \r\t]+ {return TOK_WHITESPACE;}
	"\x00" {return TOK_END;}
	[^] {return TOK_NONE;}
	*/
}

/**
 * Tokenize a `string` using `readToken()`, and return an array of all the
 * resultant `Token_t`s. The number of tokens will be stored in
 * `*numTotalTokens`. Note that `src` is expected to be null-terminated.
 */
Token_t *tokenize(const char *src, int *numTotalTokens){
	int tokenBufLength = 100;
	Token_t *tokens = malloc(tokenBufLength * sizeof(Token_t));
	int numTokens = 0;

	while(true){
		const char *start = src;
		TokenType_t tokType = readToken(&src);

		if(tokType == TOK_END){
			*numTotalTokens = numTokens;
			return tokens;
		}
		else if(tokType == TOK_NONE){
			free(tokens);
			*numTotalTokens = 0;
			return NULL;
		}
		else {
			Token_t *token = &tokens[numTokens++];
			token->body = start;
			token->length = src - start;
			token->type = tokType;
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
