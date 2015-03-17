#include <stdio.h>
#include <pcre.h>
#include <string.h>

typedef enum {TOK_WORD, TOK_NUMBER, TOK_NONE, TOK_WHITESPACE} TokenType_t;
typedef struct {
	const char *body;
	int length;
	TokenType_t type;
} Token_t;
pcre *g_tokenRegexes;

pcre *WORD_REGEX, *NUMBER_REGEX, *WHITESPACE_REGEX;

int getToken(const char *src, int length, Token_t *token){
	int matchOffsets[3];
	TokenType_t tokenType = TOK_NONE;
	if(pcre_exec(WORD_REGEX, NULL, src, length, 0, 0, matchOffsets, 6) != PCRE_ERROR_NOMATCH){
		tokenType = TOK_WORD;
	}

	else if(pcre_exec(NUMBER_REGEX, NULL, src, length, 0, 0, matchOffsets, 6) != PCRE_ERROR_NOMATCH){
		tokenType = TOK_NUMBER;
	}

	else if(pcre_exec(WHITESPACE_REGEX, NULL, src, length, 0, 0, matchOffsets, 6) != PCRE_ERROR_NOMATCH){
		tokenType = TOK_WHITESPACE;
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

// TODO: use &numTokens instead of *numTokens
Token_t *getTokens(const char *src, int length, int *numTokens){
	Token_t *tokens = malloc(100 * sizeof(Token_t));
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
			(*numTokens)++;
			length -= numBytesRead;
			nextTokenPtr += numBytesRead;
		}
	}

	return tokens;
}

void printToken(const Token_t *token){
	const char *tokenName;
	switch(token->type){
		case TOK_WORD:
			tokenName = "WORD";
			break;

		case TOK_NUMBER:
			tokenName = "NUMBER";
			break;

		case TOK_WHITESPACE:
			tokenName = "WHITESPACE";
			break;

		case TOK_NONE:
			tokenName = "NONE";
			break;
	}
	// putchar('\'');
	// fwrite(token->body, token->length, 1, stdout);
	// fputs("', ", stdout);
	printf("%s: `", tokenName);
	fwrite(token->body, token->length, 1, stdout);
	fputs("`\n", stdout);
}

int main(){
	const char *errMsg;
	int errOffset;
	const char *regexStr = "^[0-9]+";
	NUMBER_REGEX = pcre_compile(regexStr, 0, &errMsg, &errOffset, NULL);
	if(NUMBER_REGEX == NULL){
		printf(
			"`%s`. Failed to compile regular expression `%s` at index `%d`.\n",
			errMsg, regexStr, errOffset
		);
		return 1;
	}

	regexStr = "^[a-zA-Z]+";
	WORD_REGEX = pcre_compile(regexStr, 0, &errMsg, &errOffset, NULL);
	if(WORD_REGEX == NULL){
		printf(
			"`%s`. Failed to compile regular expression `%s` at index `%d`.\n",
			errMsg, regexStr, errOffset
		);
		return 1;
	}

	regexStr = "^[ \t]+";
	WHITESPACE_REGEX = pcre_compile(regexStr, 0, &errMsg, &errOffset, NULL);
	if(WHITESPACE_REGEX == NULL){
		printf(
			"`%s`. Failed to compile regular expression `%s` at index `%d`.\n",
			errMsg, regexStr, errOffset
		);
		return 1;
	}

	const char *srcStr = "302 hello what 90 20 are";
	int numTokens;
	Token_t *tokens = getTokens(srcStr, strlen(srcStr), &numTokens);
	for(int ind = 0; ind < numTokens; ind++){
		printToken(&(tokens[ind]));
	}

	pcre_free(NUMBER_REGEX);
	pcre_free(WORD_REGEX);
	pcre_free(WHITESPACE_REGEX);
	return 0;
}
