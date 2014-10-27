#include <stdio.h>
#include <stdlib.h>

#include "des.h"

/**
 * @brief Left-rotate the first 28 bits of 4 8-bit bytes. The last 4 bits are
 *      passed over when transferring bits shifted off the left end to the
 *      right end.
 * @param bytes An array of four bytes (assumed to have eight bits each).
 * @param rotDist The number of bits to rotate the array by.
 */
static void _rotLeft(Byte_t *bytes, int rotDist);

Byte_t *DES_encipher(const Byte_t *plaintext, Byte_t *key){
	return key;
}

static void _rotLeft(Byte_t *bytes, int rotDist){
	Byte_t frontShiftedBits = bytes[0] >> (8 - rotDist);
	bytes[0] <<= rotDist;

	int byte;
	for(byte = 1; byte < 4; byte++){
		Byte_t shiftedBits = bytes[byte] >> (8 - rotDist);
		bytes[byte - 1] |= shiftedBits;
		bytes[byte] <<= rotDist;
	}
	bytes[byte - 1] |= frontShiftedBits << 4;
}
