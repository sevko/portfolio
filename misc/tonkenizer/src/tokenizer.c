#include <stdio.h>
#include <pcre.h>
#include <string.h>
#include <stdlib.h>

#include "src/tokenizer.h"

static pcre **g_regexes;
static int numRegexes = 0;

/**
 * Given a string `src` of length `length`, attempt extracting a token from it.
 * If one is found, the length of the token's body will be returned (ie
 * `token->length`), and the token itself will be stored in `*token`.
 * Otherwise, returns -1.
 */
static int getToken(const char *src, int length, Token_t *token){
	#define TRY_REGEX(tokType) \
		if(pcre_exec(tokType##_REGEX, NULL, src, length, 0, PCRE_ANCHORED, matchOffsets, 6) !=\
			PCRE_ERROR_NOMATCH){ \
			tokenType = TOK_##tokType; \
		}

	int matchOffsets[3];
	TokenType_t tokenType = TOK_NONE;

	for(int ind = 0; ind < numRegexes; ind++){
		int match = pcre_exec(
			g_regexes[ind], NULL, src, length, 0, PCRE_ANCHORED,
			matchOffsets, 6
		);
		if(match != PCRE_ERROR_NOMATCH){
			tokenType = ind;
			break;
		}
	}

	if(tokenType != TOK_NONE){
		token->type = tokenType;
		token->body = src;
		token->length = matchOffsets[1];
		return token->length;
	}
	else {
		return -1;
	}
}

Token_t *getTokens(const char *src, int length, int *numTokens){
	int tokenBufferLength = 100;
	Token_t *tokens = malloc(tokenBufferLength * sizeof(Token_t));
	*numTokens = 0;
	const char *nextTokenPtr = src;

	while(length > 0){
		int numBytesRead = getToken(nextTokenPtr, length, &(tokens[*numTokens]));
		if(numBytesRead == -1){
			fputs("Failed to tokenize string at: `", stderr);
			fwrite(nextTokenPtr, length < 20 ? length : 20, 1, stderr);
			fputs("`\n", stderr);
			return NULL;
		}
		else {
			if(++(*numTokens) == tokenBufferLength){
				tokenBufferLength *= 2;
				tokens = realloc(tokens, tokenBufferLength * sizeof(Token_t));
			}

			length -= numBytesRead;
			nextTokenPtr += numBytesRead;
		}
	}

	return tokens;
}

void printToken(const Token_t *token){
	#define ENUM_CASE(name) \
		case TOK_##name: \
			tokenName = #name; \
			break;

	const char *tokenName;
	switch(token->type){
		ENUM_CASE(WORD)
		ENUM_CASE(NUMBER)
		ENUM_CASE(WHITESPACE)
		ENUM_CASE(NONE)
		ENUM_CASE(KEYWORD)
		ENUM_CASE(SYMBOL)
	}
	printf("%s: `", tokenName);
	fwrite(token->body, token->length, 1, stdout);
	fputs("`\n", stdout);
}

pcre *initRegex(const char *regexStr){
	int errOffset;
	const char *errMsg;
	pcre *regex = pcre_compile(regexStr, 0, &errMsg, &errOffset, NULL);
	if(regex == NULL){
		fprintf(
			stderr,
			"`%s`. Failed to compile regular expression `%s` at index `%d`.\n",
			errMsg, regexStr, errOffset
		);
	}
	return regex;
}

int initTokenizer(){
	numRegexes = 5;
	const char *regexStrings[numRegexes];
	regexStrings[TOK_NUMBER] = "[0-9]+";
	regexStrings[TOK_WORD] = "[a-zA-Z_]+";
	regexStrings[TOK_WHITESPACE] = "[ \t\n]+";
	regexStrings[TOK_KEYWORD] = "int|bool|char";
	regexStrings[TOK_SYMBOL] = "[{}\\[\\]().,;+*/&|<>=~-]";

	g_regexes = malloc(numRegexes * sizeof(pcre *));
	for(int ind = 0; ind < numRegexes; ind++){
		pcre *regex = initRegex(regexStrings[ind]);
		if(regex == NULL){
			return 0;
		}
		else {
			g_regexes[ind] = regex;
		}
	}
	return 1;
}

void deinitTokenizer(){
	for(int ind = 0; ind < numRegexes; ind++){
		free(g_regexes[ind]);
	}
	free(g_regexes);
}
