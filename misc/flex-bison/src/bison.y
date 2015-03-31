%{
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <error.h>

#define PRINTFERR(...) fprintf(stderr, __VA_ARGS__)

/**
 * Represents the contents of a statement parsed by bison.
 */
typedef struct {
	enum {STMT_HEADER, STMT_TYPE, STMT_BODY, STMT_FOOTER} type;
	union {
		struct {
			float version;
		} header;
		struct {
			char *name;
		} type;
		struct {
			int values[4];
			char *name;
		} body;
	} stmt;
} Statement_t;

extern int lineNum;
extern int yylex();
extern int yyparser();
extern FILE *yyin;
void yyerror(const char *err);

/**
 * Variables used to store a buffer of the `Statement_t`s parsed by bison.
 */
Statement_t *stmts;
int stmtsBufLength = 10;

/**
 * Will contain the total number of parsed statements contained in `stmts`
 * after bison finishes parsing the input.
 */
int numStmts = 0;

/**
 * Add `stmt` to `stmts`.
 */
void addStmt(Statement_t stmt){
	stmts[numStmts++] = stmt;
	if(numStmts == stmtsBufLength){
		stmtsBufLength *= 2;
		stmts = realloc(stmts, stmtsBufLength * sizeof(Statement_t));
	}
}

/**
 * Free all of the members contained inside `*stmt`, like `char *`. `stmt`
 * itself is /not/ free'd.
 */
void freeStmtMembers(Statement_t *stmt){
	switch(stmt->type){
		case STMT_TYPE:
			free(stmt->stmt.type.name);
			break;

		case STMT_BODY:
			free(stmt->stmt.body.name);
			break;
	}
}

/**
 * Return a string representation of `stmt` that has to be `free()`'d after
 * use.
 */
char *stmtToString(Statement_t *stmt){
	char *str;
	switch(stmt->type){
		case STMT_HEADER:
			asprintf(&str, "Header: version %f", stmt->stmt.header.version);
			break;

		case STMT_TYPE:
			asprintf(&str, "Type: %s", stmt->stmt.type.name);
			break;

		case STMT_BODY:
			asprintf(
				&str, "Type: %s [%d, %d, %d, %d]",
				stmt->stmt.body.name,
				stmt->stmt.body.values[0],
				stmt->stmt.body.values[1],
				stmt->stmt.body.values[2],
				stmt->stmt.body.values[3]
			);
			break;

		case STMT_FOOTER:
			str = strdup("footer");
			break;
	}

	return str;
}

void yyerror(const char *err){
	PRINTFERR("Err: line %d: %s\n", lineNum + 1, err);
	exit(1);
}

%}

%union {
	int ival;
	float fval;
	char *sval;
}

%token <ival> INT
%token <fval> FLOAT
%token <sval> STRING
%token SNAZZLE TYPE
%token END ENDL

%%

snazzle:
	header template body_section footer
	;

header:
	SNAZZLE FLOAT ENDL {
		addStmt((Statement_t){
			.type = STMT_HEADER,
			.stmt = {
				.header = {
					.version = $2
				}
			}
		});
	}
	;

template:
	template typeline
	| typeline
	;

typeline:
	TYPE STRING ENDL {
		addStmt((Statement_t){
			.type = STMT_TYPE,
			.stmt = {
				.type = {
					.name = $2
				}
			}
		});
	}
	;

body_section:
	body_section body_line
	| body_line
	;

body_line:
	INT INT INT INT STRING ENDL {
		addStmt((Statement_t){
			.type = STMT_BODY,
			.stmt = {
				.body = {
					.values = {$1, $2, $3, $4},
					.name = $5
				}
			}
		});
	}

footer:
	END ENDL {
		addStmt((Statement_t){
			.type = STMT_FOOTER
		});
	};

%%

int main(){
	const char *filePath = "corpus.txt";
	FILE *file = fopen(filePath, "r");
	if(file == NULL){
		char *buf = malloc(1000);
		PRINTFERR(
			"Couldn't open file `%s` with `%s`.\n", filePath, strerror(errno)
		);
		return -1;
	}
	yyin = file;
	stmts = malloc(stmtsBufLength * sizeof(Statement_t));
	do {
		yyparse();
	} while (!feof(yyin));

	printf("Num stmts: %d\n", numStmts);
	for(int ind = 0; ind < numStmts; ind++){
		Statement_t *stmt = &stmts[ind];
		char *str = stmtToString(stmt);
		puts(str);
		free(str);
		freeStmtMembers(stmt);
	}
	free(stmts);
	yylex_destroy();
	fclose(file);
}
