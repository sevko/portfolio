/**
 * Unit tests for the JSON parser.
 */

#include <stdio.h>
#include <stdlib.h>
#include <tap.h>
#include <string.h>

#include "src/json_parser.h"

/**
 * Test whether parsing `inputStr` throws an error of type `type`.
 */
static void testBadInput(const char *inputStr, JsonParserErrorType_t type){
	note("Testing bad input `%s`\n", inputStr);
	bool failed;
	JsonParserError_t error;
	parse(inputStr, true, strlen(inputStr), &failed, &error);
	ok(failed, "Boolean set to indicate failures.");

	if(error.type == type){
		pass("Error type matches expected.");
	}
	else {
		fail(
			"Expected error `%s` but got `%s`.", JsonErrorType_toString(type),
			JsonErrorType_toString(error.type));
	}
	JsonParserError_free(&error);
}

/**
 * Test whether the parser correctly identifies errors.
 */
static void testBadInputs(void){
	testBadInput("{\"a}", JSON_ERR_EOF);
	testBadInput("e", JSON_ERR_VALUE);
	testBadInput("1.]", JSON_ERR_NUMBER);
	testBadInput("[1.0, \"abc\", 1.]", JSON_ERR_NUMBER);
	testBadInput("5942.a", JSON_ERR_NUMBER);
	testBadInput(
		"{\"a\":123,\"b\":[[[{}, [{{]], 1, {}]}", JSON_ERR_UNEXPECTED_CHAR);
	testBadInput(
		"[\"a\", \"b\", \"c\n\"]", JSON_ERR_STR_CONTROL_CHAR);
	testBadInput(
		"\"\\u434x\"", JSON_ERR_STR_UNICODE_ESCAPE);
	testBadInput("ne   ", JSON_ERR_UNEXPECTED_CHAR);
	testBadInput("nul", JSON_ERR_EOF);
	testBadInput("{\"a\": [1], 1:4}", JSON_ERR_UNEXPECTED_CHAR);
	testBadInput("falsd", JSON_ERR_UNEXPECTED_CHAR);
	testBadInput("\"\\9\"", JSON_ERR_STR_INVALID_ESCAPE);
}

int main(){
	testBadInputs();
	return EXIT_SUCCESS;
}
