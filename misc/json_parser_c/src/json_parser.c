// Define _GNU_SOURCE to silence warnings about an implicit declaration of
// `asprintf()`.
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <setjmp.h>
#include <string.h>

#include "json_parser.h"
#include "src/stretchy_buffer.h"

typedef struct {
	const char *inputStr;
	int stringInd;
	bool isNullTerminated;
	int inputStrLength;

	int colNum;
	int lineNum;

	bool failedParse;
	JsonParserError_t error;

	jmp_buf errorTrap;
} JsonParser_t;

static JsonVal_t JsonParser_parseValue(JsonParser_t *state);

static void *copyJmpBuf(jmp_buf dest, const jmp_buf src){
	return memcpy(dest, src, sizeof(jmp_buf));
}

void JsonParserError_free(JsonParserError_t *err){
	free(err->errMsg);
}

const char *JsonErrorType_toString(JsonParserErrorType_t type){
	#define CASE(errType) \
		case errType: \
			return #errType

	switch(type){
		CASE(JSON_ERR_EOF);
		CASE(JSON_ERR_UNEXPECTED_CHAR);
		CASE(JSON_ERR_STR_UNICODE_ESCAPE);
		CASE(JSON_ERR_STR_INVALID_ESCAPE);
		CASE(JSON_ERR_STR_CONTROL_CHAR);
		CASE(JSON_ERR_BOOL);
		CASE(JSON_ERR_NUMBER);
		CASE(JSON_ERR_VALUE);

		default:
			return "Undefined type.";
	}
}

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

bool JsonVal_eq(JsonVal_t *a, JsonVal_t *b){
	if(a->type != b->type){
		return false;
	}

	switch(a->type){
		case JSON_STRING:{
			JsonString_t *aStr = &a->value.string,
				*bStr = &b->value.string;
			int aLength = aStr->length;
			return aLength == bStr->length &&
				strncmp(aStr->str, bStr->str, aLength) == 0;
		}

		case JSON_INT:
			return a->value.intNum == b->value.intNum;

		case JSON_FLOAT:{
			float diff = a->value.floatNum - b->value.floatNum;
			if(diff < 0){
				diff = -diff;
			}
			return diff <= 1e-6;
		}

		case JSON_OBJECT:{
			JsonObject_t *aObj = &a->value.object,
				*bObj = &b->value.object;
			if(aObj->length != bObj->length){
				return false;
			}

			for(int pair = 0; pair < aObj->length; pair++){
				JsonString_t *key1 = aObj->keys + pair,
					*key2 = bObj->keys + pair;
				int key1Length = key1->length;
				bool keysMatch = key1Length == key2->length &&
					strncmp(key1->str, key2->str, key1Length) == 0;
				if(!(keysMatch &&
					JsonVal_eq(aObj->values + pair, bObj->values + pair))){
					return false;
				}
			}

			break;
		}

		case JSON_ARRAY:{
			JsonArray_t *aArray = &a->value.array,
				*bArray = &b->value.array;
			if(aArray->length != bArray->length){
				return false;
			}

			for(int ind = 0; ind < aArray->length; ind++){
				if(!JsonVal_eq(aArray->values + ind, bArray->values + ind)){
					return false;
				}
			}

			break;
		}

		case JSON_BOOL:{
			bool aBool = a->value.boolean,
				bBool = b->value.boolean;
			return (aBool && bBool) || (!aBool && !bBool);
		}

		case JSON_NULL:
			break;
	}

	return true;
}

/**
 * Raise en error in `state`, setting its error message to `errMsg` with some
 * additional, helpful context (like the line and column numbers of where it
 * occurred). If `jump` is true, also jump to the previous error-catching
 * context (`state->errorTrap`).
 */
static void JsonParser_error(
	JsonParser_t *state, JsonParserErrorType_t errorType,
	char *errMsg, bool jump){
	state->failedParse = true;

	char *fullErMsg;
	int asprintfRes = asprintf(
		&fullErMsg, "Parse error on line %d, column %d:\n%s\n",
		state->lineNum, state->colNum, errMsg);
	if(asprintfRes == -1){
		fputs("JsonParser_error(): `asprintf()` call failed!", stderr);
		fullErMsg = "";
	}

	state->error = (JsonParserError_t){
		.lnNum = state->lineNum,
		.colNum = state->colNum,
		.errMsg = fullErMsg,
		.type = errorType
	};

	if(jump){
		longjmp(state->errorTrap, 1);
	}
}

static char JsonParser_peek(JsonParser_t *state){
	return state->inputStr[state->stringInd];
}

/**
 * Return the next character in the input string and advance the parser.
 */
static char JsonParser_next(JsonParser_t *state){
	if((state->isNullTerminated && !state->inputStr[state->stringInd]) ||
		state->stringInd == state->inputStrLength){
		JsonParser_error(
			state, JSON_ERR_EOF, "Unexpected end of input.", true);
	}
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
	char c = JsonParser_peek(state);
	while(c == ' ' || c == '\t' || c == '\n'){
		JsonParser_next(state);
		if(c == '\n'){
			state->colNum = 1;
			state->lineNum++;
		}
		else {
			state->colNum++;
		}
		c = JsonParser_peek(state);
	}
}

static char JsonParser_expect(JsonParser_t *state, char expected){
	char c = JsonParser_next(state);
	if(c == expected){
		return c;
	}
	else {
		char *errMsg;
		if(asprintf(
			&errMsg, "Expecting `%c`, but got `%c`.", expected, c) == -1){
			fputs("JsonParser_expect(): `asprintf()` call failed!", stderr);
			errMsg = "";
		}
		JsonParser_error(state, JSON_ERR_UNEXPECTED_CHAR, errMsg, false);
		free(errMsg);
		longjmp(state->errorTrap, 1);
	}
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
}

static JsonString_t JsonParser_parseString(JsonParser_t *state){
	JsonParser_expect(state, '"');
	char *volatile str = NULL;

	jmp_buf prevErrorTrap;
	copyJmpBuf(prevErrorTrap, state->errorTrap);
	if(!setjmp(state->errorTrap)){
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

					const char *inputStrPtr =
						&state->inputStr[state->stringInd];
					int numCharsRead;
					int numItemsMatched = sscanf(
						inputStrPtr, "%4x%n", &unicodeCodePoint, &numCharsRead);
					if(numItemsMatched != 1 || numCharsRead != 4){
						JsonParser_error(
							state, JSON_ERR_STR_UNICODE_ESCAPE,
							"Failed to read 4 hexadecimal characters", false);
						goto error;
					}
					state->stringInd += 4;

					char unicodeChr[4];
					int numBytes = 0;
					encodeUtf8CodePoint(unicodeCodePoint, &numBytes, unicodeChr);
					for(int byte = 0; byte < numBytes; byte++){
						sb_push(str, unicodeChr[byte]);
					}
				}
				else {
					JsonParser_error(
						state, JSON_ERR_STR_INVALID_ESCAPE,
						"Invalid escaped character.", false);
					goto error;
				}
			}
			else if(iscntrl(chr)){
				JsonParser_error(
					state, JSON_ERR_STR_CONTROL_CHAR,
					"Control characters inside strings are invalid.", false);
				goto error;
			}
			else {
				sb_push(str, chr);
			}
		}
		JsonParser_expect(state, '"');
		copyJmpBuf(state->errorTrap, prevErrorTrap);
		return (JsonString_t){
			.length = sb_count(str),
			.str = str
		};
	}

error:
	sb_free(str);
	longjmp(prevErrorTrap, 1);
}

static unsigned int JsonParser_parseDigits(JsonParser_t *state){
	unsigned int val;
	int numCharsRead;
	const char *srcStr = &state->inputStr[state->stringInd];
	if(sscanf(srcStr, "%d%n", &val, &numCharsRead) != 1){
		JsonParser_error(
			state, JSON_ERR_NUMBER, "Failed to read one or more digits.", true);
	}
	state->stringInd += numCharsRead;
	return val;
}

static void JsonParser_parseNumber(JsonParser_t *state, JsonVal_t *val){
	bool negate = JsonParser_nextIfChr(state, '-');
	int baseNum = JsonParser_parseDigits(state);

	bool hasFraction = JsonParser_nextIfChr(state, '.');
	int fractionNum = 0;
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
	JsonParser_expect(state, '{');

	JsonParser_skipWhitespace(state);
	if(JsonParser_peek(state) == '}'){
		JsonParser_next(state);
		return (JsonObject_t){
			.length = 0,
			.keys = NULL,
			.values = NULL
		};
	}

	JsonString_t *volatile keys = NULL;
	JsonVal_t *volatile values = NULL;

	jmp_buf prevErrorTrap;
	copyJmpBuf(prevErrorTrap, state->errorTrap);

	if(!setjmp(state->errorTrap)){
		do {
			JsonParser_skipWhitespace(state);
			JsonString_t key = JsonParser_parseString(state);
			sb_push(keys, key);

			JsonParser_skipWhitespace(state);
			JsonParser_expect(state, ':');
			JsonParser_skipWhitespace(state);

			JsonVal_t value = JsonParser_parseValue(state);
			sb_push(values, value);
		} while(JsonParser_nextIfChr(state, ','));

		JsonParser_skipWhitespace(state);
		JsonParser_expect(state, '}');
		copyJmpBuf(state->errorTrap, prevErrorTrap);
		return (JsonObject_t){
			.length = sb_count(keys),
			.keys = keys,
			.values = values
		};
	}
	else {
		// We iterate over `keys` and `values` separately since the number of
		// keys and values might differ by 1 if, for a given key-value pair, a
		// key was successfully parsed but the value parse failed.
		for(int pair = 0; pair < sb_count(keys); pair++){
			sb_free(keys[pair].str);
		}

		for(int pair = 0; pair < sb_count(values); pair++){
			JsonVal_free(&values[pair]);
		}
		sb_free(keys);
		sb_free(values);
		longjmp(prevErrorTrap, 1);
	}
}

static JsonArray_t JsonParser_parseArray(JsonParser_t *state){
	JsonParser_expect(state, '[');

	JsonParser_skipWhitespace(state);
	if(JsonParser_peek(state) == ']'){
		JsonParser_next(state);
		return (JsonArray_t){
			.length = 0,
			.values = NULL
		};
	}

	jmp_buf prevErrorTrap;
	copyJmpBuf(prevErrorTrap, state->errorTrap);

	JsonVal_t *volatile values = NULL;
	if(!setjmp(state->errorTrap)){
		do {
			JsonParser_skipWhitespace(state);
			JsonVal_t val = JsonParser_parseValue(state);
			sb_push(values, val);
			JsonParser_skipWhitespace(state);
		} while(JsonParser_nextIfChr(state, ','));

		JsonParser_expect(state, ']');
		copyJmpBuf(state->errorTrap, prevErrorTrap);
		return (JsonArray_t){
			.length = sb_count(values),
			.values = values
		};
	}
	else {
		for(int ind = 0; ind < sb_count(values); ind++){
			JsonVal_free(&values[ind]);
		}
		sb_free(values);
		longjmp(prevErrorTrap, 1);
	}
}

static JsonBool_t JsonParser_parseBoolean(JsonParser_t *state){
	switch(JsonParser_peek(state)){
		case 't':
			JsonParser_expect(state, 't');
			JsonParser_expect(state, 'r');
			JsonParser_expect(state, 'u');
			JsonParser_expect(state, 'e');
			return true;
			break;

		case 'f':
			JsonParser_expect(state, 'f');
			JsonParser_expect(state, 'a');
			JsonParser_expect(state, 'l');
			JsonParser_expect(state, 's');
			JsonParser_expect(state, 'e');
			return false;
			break;

		default:
			JsonParser_error(
				state, JSON_ERR_BOOL, "Expecting `t` or `f`.", true);

			// The following is necessary to silence a GCC warning about the
			// lack of a return type ("control reaches end of non-void
			// function")`. In reality, the above call to `JsonParser_error()`
			// will `longjmp()` to the last error-catching context, so this'll
			// never be reached.
			return false;
	}
}

static JsonNull_t JsonParser_parseNull(JsonParser_t *state){
	JsonParser_expect(state, 'n');
	JsonParser_expect(state, 'u');
	JsonParser_expect(state, 'l');
	JsonParser_expect(state, 'l');
	return 0;
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
		JsonParser_error(
			state, JSON_ERR_VALUE, "Couldn't parse a value.\n", true);
	}

	return val;
}

JsonVal_t parse(
	const char *src, bool isNullTerminated, int length, bool *failed,
	JsonParserError_t *error){
	JsonParser_t state = (JsonParser_t){
		.isNullTerminated = isNullTerminated,
		.inputStrLength = length,
		.colNum = 1,
		.lineNum = 1,
		.stringInd = 0,
		.inputStr = src,
		.failedParse = false
	};

	JsonVal_t parsedVal;
	if(!setjmp(state.errorTrap)){
		parsedVal = JsonParser_parseValue(&state);
		*failed = false;
	}
	else {
		*failed = true;
		*error = state.error;
	}
	return parsedVal;
}
