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
	JsonVal_t *vals;
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
	JsonParser_skipWhitespace(state);
	char c = state->inputStr[state->stringInd++];
	if(c == expected){
		return c;
	}
	else {
		state->failedParse = true;
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

JsonObject_t JsonParser_Object(JsonParser_t *state){
}

JsonArray_t JsonParser_Array(JsonParser_t *state){
}

JsonBool_t JsonParser_Boolean(JsonParser_t *state){
}

JsonNull_t JsonParser_Null(JsonParser_t *state){
}


JsonVal_t *JsonParser_parseValue(JsonParser_t *state){
	char peekedChar = JsonParser_peek(state);
	JsonVal_t *val = malloc(sizeof(JsonVal_t));
	return val;
}

int main(){
	puts("Hello world.");
	return EXIT_SUCCESS;
}
