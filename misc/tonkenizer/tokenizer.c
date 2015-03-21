#include <stdio.h>
#include <pcre.h>
#include <string.h>

typedef enum {
	TOK_WORD, TOK_NUMBER, TOK_NONE, TOK_WHITESPACE, TOK_SYMBOL, TOK_KEYWORD
} TokenType_t;

typedef struct {
	const char *body;
	int length;
	TokenType_t type;
} Token_t;

pcre *WORD_REGEX, *NUMBER_REGEX, *WHITESPACE_REGEX, *KEYWORD_REGEX,
	*SYMBOL_REGEX;

int getToken(const char *src, int length, Token_t *token){
	#define TRY_REGEX(tokType) \
		if(pcre_exec(tokType##_REGEX, NULL, src, length, 0, PCRE_ANCHORED, matchOffsets, 6) !=\
			PCRE_ERROR_NOMATCH){ \
			tokenType = TOK_##tokType; \
		}

	int matchOffsets[3];
	TokenType_t tokenType = TOK_NONE;

	TRY_REGEX(KEYWORD)
	else TRY_REGEX(WORD)
	else TRY_REGEX(NUMBER)
	else TRY_REGEX(WHITESPACE)
	else TRY_REGEX(SYMBOL)

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

char *readFile(const char *filePath, int *fileLength){
	FILE *file = fopen(filePath, "r");
	fseek(file, 0, SEEK_END);
	int length = ftell(file);
	rewind(file);
	char *contents = malloc(length);
	if(fread(contents, 1, length, file) != length){
		fprintf(stderr, "Could not read all of file: %s\n", filePath);
		exit(1);
	}
	fclose(file);
	*fileLength = length;
	return contents;
}

int main(){
	#define INIT_REGEX(name, regexStr) \
		name##_REGEX = pcre_compile(regexStr, 0, &errMsg, &errOffset, NULL); \
		if(name##_REGEX == NULL){ \
			printf( \
				"`%s`. Failed to compile regular expression `%s` at index `%d`.\n", \
				errMsg, regexStr, errOffset \
			); \
			return 1; \
		}

	const char *errMsg;
	int errOffset;

	INIT_REGEX(NUMBER, "[0-9]+")
	INIT_REGEX(WORD, "[a-zA-Z_]+")
	INIT_REGEX(WHITESPACE, "[ \t\n]+")
	INIT_REGEX(
		KEYWORD,
		"int|bool|char"
	)
	INIT_REGEX(SYMBOL, "[{}\\[\\]().,;+*/&|<>=~-]")

	int fileLength;
	char *corpus = readFile("corpus.txt", &fileLength);
	int numTokens;
	Token_t *tokens = getTokens(corpus, fileLength, &numTokens);
	for(int ind = 0; ind < numTokens; ind++){
		printToken(&(tokens[ind]));
	}
	free(corpus);
	free(tokens);

	pcre_free(SYMBOL_REGEX);
	pcre_free(KEYWORD_REGEX);
	pcre_free(NUMBER_REGEX);
	pcre_free(WORD_REGEX);
	pcre_free(WHITESPACE_REGEX);

	return 0;
}
