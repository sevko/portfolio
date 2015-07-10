#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>

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

#define ERROR(msg) \
	JsonParser_error(state, msg); \
	return

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
 * Recursively deallocate the members of `*val`. `val` itself will NOT be
 * `free()`'d.
 */
void JsonVal_free(JsonVal_t *val){
	switch(val->type){
		case JSON_STRING:
			sb_free(val->value.string.str);
			break;

		case JSON_OBJECT:{
			JsonObject_t obj = val->value.object;
			for(int pair = 0; pair < val->value.object.length; pair++){
				sb_free(obj.keys[pair].str);
				JsonVal_free(&obj.values[pair]);
			}
			sb_free(obj.keys);
			sb_free(obj.values);
			break;
		}

		case JSON_ARRAY:{
			JsonArray_t arr = val->value.array;
			for(int ind = 0; ind < val->value.array.length; ind++){
				JsonVal_free(&arr.values[ind]);
			}
			sb_free(arr.values);
			break;
		}

		default:
			return;
	}
}

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
		&state->errMsg, "Parse error on line %d, column %d:\n%s\n",
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

/**
 * Write a UTF8 representation of `codePoint` to `dest`, storing the number of
 * bytes it occupies (anywhere between 1 and 4 inclusive) in `*numBytes`.
 */
static void encodeUtf8CodePoint(int codePoint, int *numBytes, char *dest){
	#define SIX_BIT_BLOCK(chrInt) (((chrInt) | (1 << 7)) & ~(1 << 6))

	if(0x0000 <= codePoint && codePoint <= 0x007F){
		*numBytes = 1;
		dest[0] = codePoint;
	}

	else if(0x0080 <= codePoint && codePoint <= 0x07FF){
		*numBytes = 2;
		dest[0] = ((codePoint >> 6) | (3 << 6)) & ~(1 << 5);
		dest[1] = SIX_BIT_BLOCK(codePoint);
	}

	else if(0x0800 <= codePoint && codePoint <= 0xFFFF){
		*numBytes = 3;
		dest[0] = ((codePoint >> 12) | (7 << 5)) & ~(1 << 4);
		dest[1] = SIX_BIT_BLOCK(codePoint >> 6);
		dest[2] = SIX_BIT_BLOCK(codePoint);
	}

	else if(0x10000 <= codePoint && codePoint <= 0x1FFFFF){
		*numBytes = 4;
		dest[0] = ((codePoint >> 18) | (7 << 5)) & ~(1 << 4);
		dest[1] = SIX_BIT_BLOCK(codePoint >> 12);
		dest[2] = SIX_BIT_BLOCK(codePoint >> 6);
		dest[3] = SIX_BIT_BLOCK(codePoint);
	}

	return dest;
}

static JsonString_t JsonParser_parseString(JsonParser_t *state){
	EXPECT('"');
	char *str = NULL;
	while(JsonParser_peek(state) != '"'){
		char chr = JsonParser_next(state);
		if(chr == '\\'){
			bool escapedCntrlChr = true;
			char escapedChar = JsonParser_next(state);
			char replacementChar;

			// For brevity.
			#define ESCAPED_REPLACEMENT(escaped, replacement) \
				case escaped: \
					replacementChar = replacement; \
					break

			switch(escapedChar){
				ESCAPED_REPLACEMENT('"', '"');
				ESCAPED_REPLACEMENT('\\', '\\');
				ESCAPED_REPLACEMENT('/', '/');
				ESCAPED_REPLACEMENT('b', '\b');
				ESCAPED_REPLACEMENT('f', '\f');
				ESCAPED_REPLACEMENT('n', '\n');
				ESCAPED_REPLACEMENT('r', '\r');
				ESCAPED_REPLACEMENT('t', '\t');

				default:
					escapedCntrlChr = false;
					break;
			}

			if(escapedCntrlChr){
				sb_push(str, replacementChar);
			}

			else if(escapedChar == 'u'){
				int unicodeCodePoint;

				char *inputStrPtr = &state->inputStr[state->stringInd];
				int numCharsRead;
				int numItemsMatched = sscanf(
					inputStrPtr, "%4x%n", &unicodeCodePoint, &numCharsRead);
				if(numItemsMatched != 1 || numCharsRead != 4){
					ERROR("Failed to read 4 hexadecimal characters");
				}
				state->stringInd += 4;

				char unicodeChr[4];
				int numBytes;
				encodeUtf8CodePoint(unicodeCodePoint, &numBytes, unicodeChr);
				for(int byte = 0; byte < numBytes; byte++){
					sb_push(str, unicodeChr[byte]);
				}
			}
			else {
				ERROR("Invalid escaped character.");
			}
		}
		else if(iscntrl(chr)){
			ERROR("Control characters inside strings are invalid.");
		}
		else {
			sb_push(str, chr);
		}
	}
	EXPECT('"');
	return (JsonString_t){
		.length = sb_count(str),
		.str = str
	};
}

static unsigned int JsonParser_parseDigits(JsonParser_t *state){
	unsigned int val;
	int numCharsRead;
	char *srcStr = &state->inputStr[state->stringInd];
	if(sscanf(srcStr, "%d%n", &val, &numCharsRead) != 1){
		ERROR("Failed to read one or more digits.");
	}
	state->stringInd += numCharsRead;
	return val;
}

static void JsonParser_parseNumber(JsonParser_t *state, JsonVal_t *val){
	bool negate = JsonParser_nextIfChr(state, '-');
	int baseNum = JsonParser_parseDigits(state);

	bool hasFraction = JsonParser_nextIfChr(state, '.');
	int fractionNum;
	if(hasFraction){
		fractionNum = JsonParser_parseDigits(state);
	}

	bool hasExp = JsonParser_nextIfChr(state, 'e') ||
		JsonParser_nextIfChr(state, 'E');
	int expPart;
	if(hasExp){
		bool negateExp = !JsonParser_nextIfChr(state, '+') &&
			JsonParser_nextIfChr(state, '-');
		expPart = JsonParser_parseDigits(state);
		if(negateExp){
			expPart = -expPart;
		}
	}

	#define APPLY_EXP(numVar) \
		if(hasExp){ \
			for(int power = 0; power < expPart; power++){ \
				numVar *= 10; \
			} \
		}

	#define APPLY_NEGATION(numVar) \
		if(negate){ \
			numVar = -numVar; \
		}

	if(hasFraction){
		float floatVal = baseNum + fractionNum / 100.0;
		APPLY_EXP(floatVal);
		APPLY_NEGATION(floatVal);
		val->type = JSON_FLOAT;
		val->value.floatNum = floatVal;
	}
	else {
		APPLY_EXP(baseNum);
		APPLY_NEGATION(baseNum);
		val->type = JSON_INT;
		val->value.intNum = baseNum;
	}
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

	else if(peekedChar == '-' || isdigit(peekedChar)){
		JsonParser_parseNumber(state, &val);
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
