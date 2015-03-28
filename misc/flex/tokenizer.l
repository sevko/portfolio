/**
 * A simple tokenizer written using the flex scanner generator.
 */

%{
typedef enum {
	TOK_KEYWORD, TOK_IDENTIFIER, TOK_NUMBER, TOK_SYMBOL, TOK_WHITESPACE,
	TOK_NONE, TOK_END
} TokenType_t;

// A token.
typedef struct {
	char *body; // The contents of the token.
	int length; // The number of bytes in `body`.
	TokenType_t type;
} Token_t;

// Used by `addToken()` to store tokens as the flex scanner identifies them.
int tokenBufLength = 10, numTokens = 0;
Token_t *tokens;

/**
 * Store the current token in the `tokens` array, by copying `yyleng` bytes
 * from `yytext`. Resizes `tokens` as necessary.
 */
void addToken(TokenType_t type){
	Token_t *token = &tokens[numTokens++];
	token->body = malloc(yyleng);
	strncpy(token->body, yytext, yyleng);
	token->length = yyleng;
	token->type = type;

	if(numTokens == tokenBufLength){
		tokenBufLength *= 2;
		tokens = realloc(tokens, tokenBufLength * sizeof(Token_t));
	}
}
%}
%%

int|void|string addToken(TOK_KEYWORD);
[a-zA-Z_][a-zA-Z0-9_]* addToken(TOK_IDENTIFIER);
[0-9]+(\.[0-9]+)? addToken(TOK_NUMBER);
[-+/*{}()[\];=] addToken(TOK_SYMBOL);
[ \r\t\n]+ addToken(TOK_WHITESPACE);
. addToken(TOK_NONE);
<<EOF>> addToken(TOK_END); return 0;

%%

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
		CASE(NUMBER)
		CASE(SYMBOL)
		CASE(WHITESPACE)
		CASE(NONE)
		CASE(END)

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

int main(void){
	tokens = malloc(tokenBufLength * sizeof(Token_t));
	yylex();
	for(int ind = 0; ind < numTokens; ind++){
		Token_t *tok = &tokens[ind];
		char *tokStr = tokenToString(tok);
		puts(tokStr);
		free(tokStr);
		free(tok->body);
	}

	free(tokens);
	yylex_destroy();
	return 0;
}
