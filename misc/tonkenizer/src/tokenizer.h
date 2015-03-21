#pragma once

typedef enum {
	TOK_WORD, TOK_NUMBER, TOK_WHITESPACE, TOK_SYMBOL, TOK_KEYWORD, TOK_NONE
} TokenType_t;

typedef struct {
	const char *body;
	int length;
	TokenType_t type;
} Token_t;

Token_t *getTokens(const char *src, int length, int *numTokens);
int initTokenizer();
void deinitTokenizer();
