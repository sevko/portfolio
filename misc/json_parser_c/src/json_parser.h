/**
 * A simple, recursive-descent JSON parser.
 */

#pragma once

#include <stdbool.h>

/**
 * The following types are used to represent JSON values. `JsonVal_t` is the
 * highest-level type and aggregates all the others in a union.
 */

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

// Forward declare `JsonVal` to use it inside the `JsonObject_t` and
// `JsonArray_t` definitions.
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

// Convenience macro for creating `JsonVal_t` structs. Note that `__VA_ARGS__`
// is used for `.value` because the argument may contain commas that would
// otherwise be interpreted as an argument delimiters by the preprocessor.
#define CREATE_JSON_VAL(valType, ...) \
	(JsonVal_t){ \
		.type = valType, \
		.value = __VA_ARGS__ \
	}

/**
 * The following types are used to represent parser errors. We choose to expose
 * a full blown type, rather than just returning a string error message,
 * because it allows the user to easily programatically handle errors. A good
 * example is this parser's unit-test suite, which can simply check the type of
 * a parse error against the epexected type rather than having to check
 * error strings for equality (which is brittle and ugly).
 */

// The type of a parser error.
typedef enum {
	JSON_ERR_EOF,
	JSON_ERR_UNEXPECTED_CHAR,
	JSON_ERR_STR_UNICODE_ESCAPE,
	JSON_ERR_STR_INVALID_ESCAPE,
	JSON_ERR_STR_CONTROL_CHAR,
	JSON_ERR_BOOL,
	JSON_ERR_NUMBER,
	JSON_ERR_VALUE
} JsonParserErrorType_t;

// A parser error.
typedef struct {
	JsonParserErrorType_t type; // The type of the error.
	int colNum, lnNum; // The point where the error occurred.
	char *errMsg; // A user-friendly error message.
} JsonParserError_t;

/**
 * Parse a JSON value from `src`. `isNullTerminated` indicates whether the
 * string is terminated with a null-byte; if it isn't, `length` must contain
 * the number of bytes to read. `*failed` will be set to `false` if a value was
 * successfully parsed and the value returned; otherwise, `*failed` will be set
 * to `true` and an error object will be stored in `*error`.
 */
JsonVal_t parse(
	const char *src, bool isNullTerminated, int length, bool *failed,
	JsonParserError_t *error);

/**
 * Recursively deallocate a value returned by `parse()`. Note that the `val`
 * pointer itself will *not* be free'd.
 */
void JsonVal_free(JsonVal_t *val);

/**
 * Intended for debugging: recursively print the contents of `val` to stdout.
 * This will look more or less like JSON.
 */
void JsonVal_print(JsonVal_t *val);

/**
 * Recursively compare `a` and `b` for equality. Float values will be compared
 * with a small (1e-6) margin of error.
 */
bool JsonVal_eq(JsonVal_t *a, JsonVal_t *b);

/**
 * Deallocate the members of `err` (currently only `errMsg`); `err` itself will
 * *not* be free'd.
 */
void JsonParserError_free(JsonParserError_t *err);

/**
 * Return a string representation of `type`; note that it'll exactly match the
 * identifier (so `JSON_ERR_VALUE` will become `"JSON_ERR_VALUE"`).
 */
const char *JsonErrorType_toString(JsonParserErrorType_t type);
