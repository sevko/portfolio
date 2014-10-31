#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "des.h"

/**
 * @brief Generate the 16 DES subkeys from the master key.
 * @param key The master encryption key.
 * @param subkeys A `Byte_t [16][6]` subkeys array, initialized to 0, which
 *      will be populated with the 16 4-byte keys.
 */
test_static void _generateSubkeys(const Byte_t *key, Byte_t subkeys[][6]);

test_static void _expansionPermutation(Byte_t *block, const Byte_t *subkey);

/**
 * @brief Left-rotate the first 28 bits of 4 8-bit bytes. The last 4 bits are
 *      passed over when transferring bits shifted off the left end to the
 *      right end.
 * @param bytes An array of four bytes (assumed to have eight bits each).
 * @param rotDist The number of bits to rotate the array by.
 */
test_static void _rotLeft(Byte_t *bytes, int rotDist);

Byte_t *DES_encipher(const Byte_t *plaintext, const Byte_t *key){
	Byte_t subkeys[16][6] = {{0}};
	_generateSubkeys(key, subkeys);

	int initialPermutation[] = {
		58, 50, 42, 34, 26, 18, 10, 2, 60, 52, 44, 36, 28, 20, 12, 4,
		62, 54, 46, 38, 30, 22, 14, 6, 64, 56, 48, 40, 32, 24, 16, 8,
		57, 49, 41, 33, 25, 17, 9, 1, 59, 51, 43, 35, 27, 19, 11, 3,
		61, 53, 45, 37, 29, 21, 13, 5, 63, 55, 47, 39, 31, 23, 15, 7
	};

	Byte_t blocks[17][2][4] = {{{0}}};
	int bit;
	for(bit = 0; bit < 32; bit++){
		if(BitOps_getBit(plaintext, initialPermutation[bit] - 1)){
			BitOps_setBit(blocks[0][0], bit);
		}
	}

	for(; bit < 64; bit++){
		if(BitOps_getBit(plaintext, initialPermutation[bit] - 1)){
			BitOps_setBit(blocks[0][1], bit - 32);
		}
	}

	for(int block = 1; block < 17; block++){
		for(int byte = 0; byte < 4; byte++){
			blocks[block][0][byte] = blocks[block - 1][1][byte];
			blocks[block][1][byte] = blocks[block - 1][0][byte];
		}
		_expansionPermutation(blocks[block - 1][1], subkeys[block - 1]);
	}

	return (Byte_t *)plaintext;
}

test_static void _generateSubkeys(const Byte_t *key, Byte_t subkeys[][6]){
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

	static const int permutedChoice2[] = {
		14, 17, 11, 24, 1, 5, 3, 28, 15, 6, 21, 10,
		23, 19, 12, 4, 26, 8, 16, 7, 27, 20, 13, 2,
		41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
		44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32
	};
	for(int subkey = 0; subkey < 16; subkey++){
		for(bit = 0; bit < 48; bit++){
			int bitPos = permutedChoice2[bit] - 1;
			int bitVal = (bitPos < 28) ?
				BitOps_getBit(permutedBlocks[subkey + 1][0], bitPos) :
				BitOps_getBit(permutedBlocks[subkey + 1][1], bitPos - 28);
			if(bitVal){
				BitOps_setBit(subkeys[subkey], bit);
			}
		}
	}
}

test_static void _expansionPermutation(Byte_t *block, const Byte_t *subkey){
	Byte_t expandedBlock[6] = {0};
	const int expansionTable[] = {
		32, 1, 2, 3, 4, 5, 4, 5, 6, 7, 8, 9,
		8, 9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17,
		16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25,
		24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32, 1
	};

	const int sBoxes[8][4][16] = {
		{
			{14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7},
			{0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8},
			{4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0},
			{15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13}
		},
		{
			{15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10},
			{3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5},
			{0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15},
			{13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9}
		},
		{
			{10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8},
			{13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1},
			{13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7},
			{1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12}
		},
		{
			{7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15},
			{13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9},
			{10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4},
			{3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14}
		},
		{
			{2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9},
			{14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6},
			{4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14},
			{11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3}
		},
		{
			{12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11},
			{10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8},
			{9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6},
			{4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13}
		},
		{
			{4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1},
			{13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6},
			{1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2},
			{6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12}
		},
		{
			{13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7},
			{1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2},
			{7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8},
			{2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11}
		}
	};

	for(int bit = 0; bit < 48; bit++){
		if(BitOps_getBit(block, expansionTable[bit] - 1)){
			BitOps_setBit(expandedBlock, bit);
		}
	}

	BitOps_xor(expandedBlock, subkey, 6);

	int bit4Groups[8] = {0};
	for(int bit6Group = 0; bit6Group < 8; bit6Group++){
		int bitOffset = bit6Group * 6;
		int sBoxCol = (BitOps_getBit(expandedBlock, bitOffset) << 1) |
			BitOps_getBit(expandedBlock, bitOffset + 5);
		int sBoxRow = 0;

		for(int bit = 1; bit < 5; bit++){
			if(BitOps_getBit(expandedBlock, bitOffset + bit)){
				sBoxRow |= 1 << (4 - bit);
			}
		}

		bit4Groups[bit6Group] = sBoxes[bit6Group][sBoxCol][sBoxRow];
	}

	for(int byte = 0; byte < 4; byte++){
		int groupOffset = byte * 2;
		expandedBlock[byte] = (bit4Groups[groupOffset] << 4) |
			(bit4Groups[groupOffset + 1]);
	}
}

test_static void _rotLeft(Byte_t *bytes, int rotDist){
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
