#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "des.h"
#include "des_tables.h"

/**
 * @brief Generate the 16 DES subkeys from the master key.
 * @param key The master 8-byte encryption key.
 * @param subkeys A `Byte_t [16][6]` subkeys array, initialized to 0, which
 *      will be populated with the 16 4-byte keys.
 */
test_static void _generateSubkeys(const Byte_t *key, Byte_t subkeys[][6]);

/**
 * @brief Perform an expansion permutation and s-box substition on a block.
 * @param target The 4-byte buffer to write the results of operations to.
 *      In DES, this is the right half of an 8-byte block.
 * @param block The 4-byte block used as a seed value; in DES, the right half
 *      of the preceding 8-byte block.
 * @param subkey The subkey corresponding to this block (first subkey for the
 *      first permutation, second for second, etc.).
 */
test_static void _expansionPermutation(
	Byte_t *target, const Byte_t *block, const Byte_t *subkey
);

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
		_expansionPermutation(
			blocks[block][1], blocks[block - 1][1], subkeys[16 - block]
		);
	}

	Byte_t finalBlocks[17][8] = {{0}};
	for(int block = 0; block < 17; block++){
		for(bit = 0; bit < 64; bit++){
			int bitPos = finalPermutation[bit] - 1;
			int bitVal = (bitPos < 32) ?
				BitOps_getBit(blocks[block][1], bitPos) :
				BitOps_getBit(blocks[block][0], bitPos - 32);

			if(bitVal){
				BitOps_setBit(finalBlocks[block], bit);
			}
		}
	}

	Byte_t *ciphertext;
	if((ciphertext = malloc(8))){
		for(int byte = 0; byte < 8; byte++){
			ciphertext[byte] = finalBlocks[16][byte];
		}
	};
	return ciphertext;
}

test_static void _generateSubkeys(const Byte_t *key, Byte_t subkeys[][6]){
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

	for(int block = 1; block < 17; block++){
		memcpy(permutedBlocks[block], permutedBlocks[block - 1], 2 * 4);
		_rotLeft(permutedBlocks[block][0], leftRotSchedule[block - 1]);
		_rotLeft(permutedBlocks[block][1], leftRotSchedule[block - 1]);
	}

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

test_static void _expansionPermutation(
	Byte_t *target, const Byte_t *block, const Byte_t *subkey
){
	Byte_t expandedBlock[6] = {0};
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

	Byte_t finalBlock[4] = {0};
	for(int bit = 0; bit < 32; bit++){
		if(BitOps_getBit(expandedBlock, permutation[bit] - 1)){
			BitOps_setBit(finalBlock, bit);
		}
	}

	BitOps_xor(target, finalBlock, 4);
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
