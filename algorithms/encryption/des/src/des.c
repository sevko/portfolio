#include <string.h>
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
	static const int permutedChoice1[] = {
		57, 49, 41, 33, 25, 17, 9, 1, 58, 50, 42, 34, 26, 18,
		10, 2, 59, 51, 43, 35, 27, 19, 11, 3, 60, 52, 44, 36,
		63, 55, 47, 39, 31, 23, 15, 7, 62, 54, 46, 38, 30, 22,
		14, 6, 61, 53, 45, 37, 29, 21, 13, 5, 28, 20, 12, 4
	};

	Byte_t permutedBlocks[17][2][4] = {{{0}}};
	int bit;
	for(bit = 0; bit < 28; bit++){
		if(BitOps_getBit(key, permutedChoice1[bit] - 1)){
			BitOps_setBit(permutedBlocks[0][0], bit);
		}
	}

	for(; bit < 56; bit++){
		if(BitOps_getBit(key, permutedChoice1[bit] - 1)){
			BitOps_setBit(permutedBlocks[0][1], bit - 28);
		}
	}

	static int leftRotSchedule[] = {
		1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1
	};
	for(int block = 1; block < 17; block++){
		memcpy(permutedBlocks[block], permutedBlocks[block - 1], 2 * 4);
		_rotLeft(permutedBlocks[block][0], leftRotSchedule[block - 1]);
		_rotLeft(permutedBlocks[block][1], leftRotSchedule[block - 1]);
	}

	return (Byte_t *)plaintext;
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
