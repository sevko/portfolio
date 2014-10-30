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

int main(){
	note("Begin unit tests.");
	const Byte_t plaintext[] = {0x1, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef};
	const Byte_t key[] = {0x13, 0x34, 0x57, 0x79, 0x9b, 0xbc, 0xdf, 0xf1};
	DES_encipher(plaintext, key);
	done_testing();
	return EXIT_SUCCESS;
}
