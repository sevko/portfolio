/**
 * Unit tests for the DES module. This file acts as the main entry point of a
 * standalone executable for this project.
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <tap.h>

#include "src/des.h"

/**
 * @brief Define a unit-test for a function. Saves on boilerplate code.
 * @param funcName The name of the function the unit-test is written for.
 * @param ... The contents of the test.
 */
#define DEF_UNIT_TEST(funcName, ...) \
	static void _test_## funcName (void){\
		note("Test " #funcName ".");\
		__VA_ARGS__\
	}

/**
 * @brief Execute the unit-test for a certain function.
 * @param funcName The name of the function whose unit-test will be executed.
 *      Must have been defined with `DEF_UNIT_TEST()` earlier on.
 */
#define EXEC_UNIT_TEST(funcName) _test_ ##funcName ()

/**
 * Test `DES_encipher()`.
 */
DEF_UNIT_TEST(
	DES_encipher,
	const Byte_t key[] = {0xa0, 0x84, 0xe4, 0xf8, 0x9a, 0xb9, 0xcd, 0x1e},
		plaintext[] = {0xb1, 0x43, 0x17, 0x68, 0x4f, 0xc5, 0x3b, 0xd0},
		expected[] = {0x87, 0x8C, 0x14, 0xCF, 0xCC, 0xD1, 0xFF, 0x8C};
	Byte_t *ciphertext = DES_encipher(plaintext, key);

	bool matches = true;
	for(int byte = 0; byte < 8; byte++){
		if(ciphertext[byte] != expected[byte]){
			matches = false;
			break;
		}
	}

	free(ciphertext);
	ok(matches, "Ciphertext matches expected.");
)

/**
 * Test `DES_decipher()`.
 */
DEF_UNIT_TEST(
	DES_decipher,
	const Byte_t key[] = {0xa0, 0x84, 0xe4, 0xf8, 0x9a, 0xb9, 0xcd, 0x1e},
		ciphertext[] = {0x87, 0x8C, 0x14, 0xCF, 0xCC, 0xD1, 0xFF, 0x8C},
		expected[] = {0xb1, 0x43, 0x17, 0x68, 0x4f, 0xc5, 0x3b, 0xd0};
	Byte_t *plaintext = DES_decipher(ciphertext, key);

	bool matches = true;
	for(int byte = 0; byte < 8; byte++){
		if(plaintext[byte] != expected[byte]){
			matches = false;
			break;
		}
	}

	free(plaintext);
	ok(matches, "Plaintext matches expected.");
)

/**
 * Test `_generateSubkeys()`.
 */
DEF_UNIT_TEST(
	_generateSubkeys,
	const Byte_t key[] = {0x13, 0x34, 0x57, 0x79, 0x9b, 0xbc, 0xdf, 0xf1};
	const Byte_t expectedSubkeys[16][6] = {
		{0x1b, 0x2, 0xef, 0xfc, 0x70, 0x72},
		{0x79, 0xae, 0xd9, 0xdb, 0xc9, 0xe5},
		{0x55, 0xfc, 0x8a, 0x42, 0xcf, 0x99},
		{0x72, 0xad, 0xd6, 0xdb, 0x35, 0x1d},
		{0x7c, 0xec, 0x7, 0xeb, 0x53, 0xa8},
		{0x63, 0xa5, 0x3e, 0x50, 0x7b, 0x2f},
		{0xec, 0x84, 0xb7, 0xf6, 0x18, 0xbc},
		{0xf7, 0x8a, 0x3a, 0xc1, 0x3b, 0xfb},
		{0xe0, 0xdb, 0xeb, 0xed, 0xe7, 0x81},
		{0xb1, 0xf3, 0x47, 0xba, 0x46, 0x4f},
		{0x21, 0x5f, 0xd3, 0xde, 0xd3, 0x86},
		{0x75, 0x71, 0xf5, 0x94, 0x67, 0xe9},
		{0x97, 0xc5, 0xd1, 0xfa, 0xba, 0x41},
		{0x5f, 0x43, 0xb7, 0xf2, 0xe7, 0x3a},
		{0xbf, 0x91, 0x8d, 0x3d, 0x3f, 0xa},
		{0xcb, 0x3d, 0x8b, 0xe, 0x17, 0xf5}
	};
	Byte_t realSubkeys[16][6] = {{0}};
	_generateSubkeys(key, realSubkeys);
	for(int key = 0; key < 16; key++){
		bool keysEqual = true;
		for(int byte = 0; byte < 6; byte++){
			if(realSubkeys[key][byte] != expectedSubkeys[key][byte]){
				keysEqual = false;
				break;
			}
		}
		ok(keysEqual, "Subkey %d matches expected.", key);
	}
)

/**
 * Test `_rotLeft()`.
 */
DEF_UNIT_TEST(
	_rotLeft,
	Byte_t bytes[] = {0xa2, 0x59, 0x8d, 0x50};
	Byte_t expected1[] = {0x44, 0xb3, 0x1a, 0xb0};
	_rotLeft(bytes, 1);

	bool matchesExpected = true;
	for(int byte = 0; byte < 4; byte++){
		if(bytes[byte] != expected1[byte]){
			matchesExpected = false;
			break;
		}
	}

	ok(matchesExpected, "Test case 1 matches expected.");

	Byte_t expected2[] = {0x96, 0x63, 0x56, 0x80};
	_rotLeft(bytes, 5);
	matchesExpected = true;
	for(int byte = 0; byte < 4; byte++){
		if(bytes[byte] != expected2[byte]){
			matchesExpected = false;
			break;
		}
	}

	ok(matchesExpected, "Test case 2 matches expected.");
)

int main(){
	note("Begin unit tests.");
	EXEC_UNIT_TEST(_generateSubkeys);
	EXEC_UNIT_TEST(_rotLeft);
	EXEC_UNIT_TEST(DES_encipher);
	EXEC_UNIT_TEST(DES_decipher);
	done_testing();
	return EXIT_SUCCESS;
}
