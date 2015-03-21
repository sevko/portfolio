/*
 * @brief h
*/

#pragma once

typedef enum {
	TOK_WORD, TOK_NUMBER, TOK_NONE, TOK_WHITESPACE, TOK_SYMBOL, TOK_KEYWORD
} TokenType_t;

typedef struct {
	const char *body;
	int length;
	TokenType_t type;
} Token_t;

Token_t *getTokens(const char *src, int length, int *numTokens);
void initTokenizer();
void deinitTokenizer();
