#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "json_parser.h"

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
char *JsonVal_print(JsonVal_t *val){
	fflush(stdout);

	switch(val->type){
		case JSON_STRING:
			printf("\"%s\"\n", val->value.string);
			break;

		case JSON_INT:
			printf("%d\n", val->value.intNum);
			break;

		case JSON_FLOAT:
			printf("%f\n", val->value.floatNum);
			break;

		case JSON_OBJECT:
			putchar('{');
			for(int ind = 0; ind < val->value.object.length; ind++){
				JsonVal_print(&val->value.object.keys[ind]);
				putchar(':');
				JsonVal_print(&val->value.object.values[ind]);
			}
			putchar('}');
			break;

		case JSON_ARRAY:
			putchar('[');
			for(int ind = 0; ind < val->value.array.length; ind++){
				JsonVal_print(&val->value.array.values[ind]);
			}
			putchar(']');
			break;

		case JSON_BOOL:
			break;

		case JSON_NULL:
			break;
	}
}

static void JsonParser_error(JsonParser_t *state, const char *errMsg){
	state->failedParse = true;
	asprintf(
		&state->errMsg, "Parse error on line %d, column %d:\n%s",
		state->lineNum, state->colNum, errMsg);
}

static char JsonParser_peek(JsonParser_t *state){
	return state->inputStr[state->stringInd];
}

static void JsonParser_next(JsonParser_t *state){
	state->stringInd++;
}

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
	char c = state->inputStr[state->stringInd++];
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

JsonString_t JsonParser_String(JsonParser_t *state){
}

JsonInt_t JsonParser_IntNum(JsonParser_t *state){
}

JsonFloat_t JsonParser_FloatNum(JsonParser_t *state){
}

static JsonObject_t JsonParser_parseObject(JsonParser_t *state){
	JsonParser_expect(state, '{');
	if(state->failedParse){
		return;
	}

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
		if(state->failedParse){
			return;
		}
		sb_push(keys, key);

		JsonParser_skipWhitespace(state);
		JsonParser_expect(state, ':');
		if(state->failedParse){
			return;
		}
		JsonParser_skipWhitespace(state);

		JsonVal_t value = JsonParser_parseValue(state);
		if(state->failedParse){
			return;
		}
		sb_push(values, value);
		JsonParser_skipWhitespace(state);
	} while(0);

	return (JsonObject_t){
		.length = sb_count(keys),
		.keys = keys,
		.values = values
	};
}

static JsonArray_t JsonParser_parseArray(JsonParser_t *state){
	JsonParser_expect(state, '[');
	if(state->failedParse){
		return;
	}

	JsonParser_skipWhitespace(state);
	if(JsonParser_peek(state) == ']'){
		JsonParser_next(state);
		return (JsonArray_t){
			.length = 0,
			.values = NULL
		};
	}

	JsonVal_t *values = NULL;
	JsonVal_t val = JsonParser_parseValue(state);
	sb_push(values, val);
	JsonParser_skipWhitespace(state);

	while(JsonParser_peek(state) == ','){
		JsonParser_next(state);
		if(state->failedParse){
			return;
		}
		val = JsonParser_parseValue(state);
		sb_push(values, val);
		JsonParser_skipWhitespace(state);
	}

	JsonParser_expect(state, ']');
	if(state->failedParse){
		return;
	}
	return (JsonArray_t){
		.length = sb_count(values),
		.values = values
	};
}

JsonBool_t JsonParser_Boolean(JsonParser_t *state){
}

JsonNull_t JsonParser_Null(JsonParser_t *state){
}


static JsonVal_t JsonParser_parseValue(JsonParser_t *state){
	char peekedChar = JsonParser_peek(state);
	JsonVal_t val;

	if(JsonParser_peek(state) == '['){
		val.type = JSON_ARRAY;
		val.value.array = JsonParser_parseArray(state);
		if(state->failedParse){
			return;
		}
		return val;
	}

	else if(JsonParser_peek(state) == '{'){
		val.type = JSON_OBJECT;
		val.value.object = JsonParser_parseObject(state);
		if(state->failedParse){
			return;
		}
		return val;
	}

	JsonParser_error(state, "Couldn't parse a value.\n");
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
