#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "json_parser.h"
#include "src/stretchy_buffer.h"

#define EXPECT(expected) \
	JsonParser_expect(state, expected); \
	if(state->failedParse){ \
		return; \
	}

#define RETURN_IF_PARSE_FAILED \
	if(state->failedParse){ \
		return; \
	}

typedef enum {
	JSON_STRING,
	JSON_INT,
	JSON_FLOAT,
	JSON_OBJECT,
	JSON_ARRAY,
	JSON_BOOL,
	JSON_NULL
} JsonType_t;

typedef struct {
	int length;
	char *str;
} JsonString_t;

typedef int JsonInt_t;
typedef float JsonFloat_t;

typedef struct JsonVal JsonVal_t;
typedef struct {
	int length;
	JsonString_t *keys;
	JsonVal_t *values;
} JsonObject_t;

typedef struct {
	int length;
	JsonVal_t *values;
} JsonArray_t;

typedef bool JsonBool_t;
typedef int JsonNull_t;

struct JsonVal {
	JsonType_t type;
	union {
		JsonString_t string;
		JsonInt_t intNum;
		JsonFloat_t floatNum;
		JsonObject_t object;
		JsonArray_t array;
		JsonBool_t boolean;
		JsonNull_t null;
	} value;
};

typedef struct {
	char *inputStr;
	int stringInd;

	int colNum;
	int lineNum;

	bool failedParse;
	char *errMsg;
} JsonParser_t;

static JsonVal_t JsonParser_parseValue(JsonParser_t *state);

/**
 * Recursively print a string representation of `*val`; intended primarily for
 * debbuging.
 */
void JsonVal_print(JsonVal_t *val){
	switch(val->type){
		case JSON_STRING:
			printf("\"%.*s\"", val->value.string.length, val->value.string.str);
			break;

		case JSON_INT:
			printf("%d", val->value.intNum);
			break;

		case JSON_FLOAT:
			printf("%f", val->value.floatNum);
			break;

		case JSON_OBJECT:
			putchar('{');
			for(int ind = 0; ind < val->value.object.length; ind++){
				JsonString_t *key = &val->value.object.keys[ind];
				printf("\"%.*s\"", key->length, key->str);
				putchar(':');
				JsonVal_print(&val->value.object.values[ind]);
				if(ind < val->value.object.length - 1){
					putchar(',');
				}
			}
			putchar('}');
			break;

		case JSON_ARRAY:
			putchar('[');
			for(int ind = 0; ind < val->value.array.length; ind++){
				JsonVal_print(&val->value.array.values[ind]);
				if(ind < val->value.array.length - 1){
					putchar(',');
				}
			}
			putchar(']');
			break;

		case JSON_BOOL:
			// Use `fputs()` instead of `puts()` to avoid newline.
			fputs(val->value.boolean ? "true" : "false", stdout);
			break;

		case JSON_NULL:
			fputs("null", stdout);
			break;
	}
}

/**
 * Raise en error in `state`, setting its error message to `errMsg` with some
 * additional, helpful context (like the line and column numbers of where it
 * occurred).
 */
static void JsonParser_error(JsonParser_t *state, const char *errMsg){
	state->failedParse = true;
	asprintf(
		&state->errMsg, "Parse error on line %d, column %d:\n%s",
		state->lineNum, state->colNum, errMsg);
}

static char JsonParser_peek(JsonParser_t *state){
	return state->inputStr[state->stringInd];
}

/**
 * Advance the parser to the next character.
 */
static char JsonParser_next(JsonParser_t *state){
	char chr = state->inputStr[state->stringInd++];
	if(chr == '\n'){
		state->lineNum++;
		state->colNum = 1;
	}
	else {
		state->colNum++;
	}
	return chr;
}

/**
 * A convenience function for advancing the parser *only* if the next character
 * is `c`. This makes the definitions of `JsonParser_parseArray()` and
 * `JsonParser_parseObject()` cleaner than they otherwise would be when it
 * comes to parsing comma-delimited values.
 */
static bool JsonParser_nextIfChr(JsonParser_t *state, char c){
	bool matches = JsonParser_peek(state) == c;
	if(matches){
		JsonParser_next(state);
	}
	return matches;
}

/**
 * Advance the parser past any whitespace.
 */
static void JsonParser_skipWhitespace(JsonParser_t *state){
	char c = state->inputStr[state->stringInd];
	while(c == ' ' || c == '\t' || c == '\n'){
		if(c == '\n'){
			state->colNum = 1;
			state->lineNum++;
		}
		else {
			state->colNum++;
		}
		c = state->inputStr[++state->stringInd];
	}
}

static char JsonParser_expect(JsonParser_t *state, char expected){
	char c = JsonParser_next(state);
	if(c == expected){
		return c;
	}
	else {
		char *errMsg;
		asprintf(&errMsg, "Expecting `%c`, but got `%c`.", expected, c);
		JsonParser_error(state, errMsg);
		free(errMsg);
		return -1;
	}
}

static bool isCharDigit(char c){
	return '0' <= c && c <= '9';
}

static JsonString_t JsonParser_parseString(JsonParser_t *state){
	EXPECT('"');
	char *str = NULL;
	while(JsonParser_peek(state) != '"'){
		char chr = JsonParser_next(state);
		sb_push(str, chr);
	}
	EXPECT('"');
	return (JsonString_t){
		.length = sb_count(str),
		.str = str
	};
}

static JsonInt_t JsonParser_parseIntNum(JsonParser_t *state){
}

static JsonFloat_t JsonParser_parseFloatNum(JsonParser_t *state){
}

static JsonObject_t JsonParser_parseObject(JsonParser_t *state){
	EXPECT('{');

	JsonParser_skipWhitespace(state);
	if(JsonParser_peek(state) == '}'){
		JsonParser_next(state);
		return (JsonObject_t){
			.length = 0,
			.keys = NULL,
			.values = NULL
		};
	}

	JsonString_t *keys = NULL;
	JsonVal_t *values = NULL;

	do {
		JsonParser_skipWhitespace(state);
		JsonString_t key = JsonParser_parseString(state);
		RETURN_IF_PARSE_FAILED;
		sb_push(keys, key);

		JsonParser_skipWhitespace(state);
		EXPECT(':');
		JsonParser_skipWhitespace(state);

		JsonVal_t value = JsonParser_parseValue(state);
		RETURN_IF_PARSE_FAILED;
		sb_push(values, value);
	} while(JsonParser_nextIfChr(state, ','));

	JsonParser_skipWhitespace(state);
	EXPECT('}');
	return (JsonObject_t){
		.length = sb_count(keys),
		.keys = keys,
		.values = values
	};
}

static JsonArray_t JsonParser_parseArray(JsonParser_t *state){
	EXPECT('[');

	JsonParser_skipWhitespace(state);
	if(JsonParser_peek(state) == ']'){
		JsonParser_next(state);
		return (JsonArray_t){
			.length = 0,
			.values = NULL
		};
	}

	JsonVal_t *values = NULL;
	do {
		JsonParser_skipWhitespace(state);
		JsonVal_t val = JsonParser_parseValue(state);
		RETURN_IF_PARSE_FAILED;
		sb_push(values, val);
		JsonParser_skipWhitespace(state);
	} while(JsonParser_nextIfChr(state, ','));

	EXPECT(']');

	return (JsonArray_t){
		.length = sb_count(values),
		.values = values
	};
}

static JsonBool_t JsonParser_parseBoolean(JsonParser_t *state){
	switch(JsonParser_peek(state)){
		case 't':
			EXPECT('t');
			EXPECT('r');
			EXPECT('u');
			EXPECT('e');
			break;

		case 'f':
			EXPECT('f');
			EXPECT('a');
			EXPECT('l');
			EXPECT('s');
			EXPECT('e');
			break;

		default:
			JsonParser_error(state, "Expecting `t` or `f`.");
	}
}

static JsonNull_t JsonParser_parseNull(JsonParser_t *state){
	EXPECT('n');
	EXPECT('u');
	EXPECT('l');
	EXPECT('l');
	return NULL;
}

static JsonVal_t JsonParser_parseValue(JsonParser_t *state){
	char peekedChar = JsonParser_peek(state);
	JsonVal_t val;

	if(peekedChar == '['){
		val.type = JSON_ARRAY;
		val.value.array = JsonParser_parseArray(state);
	}

	else if(peekedChar == '{'){
		val.type = JSON_OBJECT;
		val.value.object = JsonParser_parseObject(state);
	}

	else if(peekedChar == '"'){
		val.type = JSON_STRING;
		val.value.string = JsonParser_parseString(state);
	}

	else if(peekedChar == 'f' || peekedChar == 't'){
		val.type = JSON_BOOL;
		val.value.boolean = JsonParser_parseBoolean(state);
	}

	else if(peekedChar == 'n'){
		val.type = JSON_NULL;
		val.value.null = JsonParser_parseNull(state);
	}

	else {
		JsonParser_error(state, "Couldn't parse a value.\n");
	}

	return val;
}

JsonVal_t parse(char *src, bool *failed, char **errMsg){
	JsonParser_t state = (JsonParser_t){
		.colNum = 1,
		.lineNum = 1,
		.stringInd = 0,
		.inputStr = src,
		.failedParse = false,
		.errMsg = NULL
	};

	JsonVal_t parsedVal = JsonParser_parseValue(&state);
	*failed = state.failedParse;
	*errMsg = state.errMsg;
	return parsedVal;
}

int main(){
	bool parseFailed;
	char *parseErrMsg;
	JsonVal_t parsed = parse("[{},{},[[[[]]]]]", &parseFailed, &parseErrMsg);
	if(parseFailed){
		fputs(parseErrMsg, stderr);
		exit(1);
	}
	JsonVal_print(&parsed);
	return EXIT_SUCCESS;
}
