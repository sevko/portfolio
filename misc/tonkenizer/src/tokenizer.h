/**
 * A simple regex-based tokenizer.
 */

#pragma once

typedef enum {
	TOK_WORD, TOK_NUMBER, TOK_WHITESPACE, TOK_SYMBOL, TOK_KEYWORD, TOK_NONE
} TokenType_t;

typedef struct {
	const char *body;
	int length;
	TokenType_t type;
} Token_t;

/**
 * Given a corpus text, `src`, of `length` bytes, tokenize it and return a
 * pointer to the resultant `Token_t`s. The number of tokens created will be
 * stored in `*numTokens`. Note that the `body` members of tokens in the
 * returned buffer all point to locations inside `src`, so it should not be
 * `free()`d until you're done with the tokens.
 */
Token_t *getTokens(const char *src, int length, int *numTokens);

/**
 * Must be called before using `getTokens()`.
 */
int initTokenizer();

/**
 * Must be called after you're finished with this module.
 */
void deinitTokenizer();

/**
 * Format and print `*token` to `stdout`.
 */
void printToken(const Token_t *token);
