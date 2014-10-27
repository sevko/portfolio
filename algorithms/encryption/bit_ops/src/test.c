#include <stdio.h>
#include <stdlib.h>
#include <tap.h>

#include "bit_ops.h"

#define DEF_UNIT_TEST(funcName, ...) \
	static void _test_## funcName (void){\
		note("Test " #funcName ".");\
		__VA_ARGS__\
	}

#define EXEC_UNIT_TEST(funcName) _test_ ##funcName ()

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
	EXEC_UNIT_TEST(BitOps_getBitString);
	done_testing();
	return EXIT_SUCCESS;
}
