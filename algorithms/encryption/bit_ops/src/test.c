/**
 * @brief Unit-tests for the `bit_ops` module.
 */

#include <stdio.h>
#include <stdlib.h>
#include <tap.h>

#include "bit_ops.h"

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
 * Test `BitOps_getBit()`.
 */
DEF_UNIT_TEST(
	BitOps_getBit,
	Byte_t bytes[] = {4};
	int expectedBits[] = {0, 0, 0, 0, 0, 1, 0, 0};
	for(int bit = 0; bit < 8; bit++){
		ok(
			BitOps_getBit(bytes, bit) == expectedBits[bit],
			"Bit %d matches.", bit
		);
	}
)

/**
 * Test `BitOps_setBit()`.
 */
DEF_UNIT_TEST(
	BitOps_setBit,
	Byte_t bytes[] = {0};
	BitOps_setBit(bytes, 0);
	BitOps_setBit(bytes, 2);
	BitOps_setBit(bytes, 3);
	BitOps_setBit(bytes, 7);
	int expectedBits[] = {1, 0, 1, 1, 0, 0, 0, 1};
	for(int bit = 0; bit < 8; bit++){
		ok(
			BitOps_getBit(bytes, bit) == expectedBits[bit],
			"Bit %d matches.", bit
		);
	}
)

/**
 * Test `BitOps_getBitString()`.
 */
DEF_UNIT_TEST(
	BitOps_getBitString,
	Byte_t bytes[] = {123, 134, 231};
	char *expected = "011110111000011011100111",
		*actual = BitOps_getBitString(bytes, 3);
	is(expected, actual, "Actual bit string matches expected.");
	free(actual);
)

int main(){
	note("Begin unit tests.");
	EXEC_UNIT_TEST(BitOps_getBit);
	EXEC_UNIT_TEST(BitOps_setBit);
	EXEC_UNIT_TEST(BitOps_getBitString);
	done_testing();
	return EXIT_SUCCESS;
}
