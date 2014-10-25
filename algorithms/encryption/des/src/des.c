#include <stdio.h>
#include <stdlib.h>

#include "des.h"

/**
 * @brief Set the value of a bit in a series of bytes to 1.
 * @param bytes An array of one or more bytes.
 * @param bit Any number in the range [0, (len of `bytes`) * 8 - 1]; the bits
 *      in `bytes` are zero-indexed.
 */
static inline void _setBit(Byte_t *bytes, int bit);

/**
 * @param bytes An array of one or more bytes.
 * @param bit Any number in the range [0, (len of `bytes`) * 8 - 1]; the bits
 *      in `bytes` are zero-indexed.
 * @return The value of the `bit` bit in `bytes`: 0 or 1.
 */
static inline int _getBit(const Byte_t *bytes, int bit);

/**
 * @brief For testing; will be removed.
 */
static void _printBinaryBytes(const Byte_t *bytes, int numBytes);

/**
 * @brief Helper for `_getBit()`/`_setBit()`.
 * @param bit The position of the bit (any number >= 0).
 * @return The distance to shift a value either left or right to create a mask.
 */
static inline int _shiftDist(int bit);

Byte_t *DES_encipher(const Byte_t *plaintext, Byte_t *key){
	return (Byte_t *)plaintext;
}

static inline void _setBit(Byte_t *bytes, int bit){
	bytes[bit / 8] |= 1 << _shiftDist(bit);
}

static inline int _getBit(const Byte_t *bytes, int bit){
	return ((bytes[bit / 8] >> _shiftDist(bit))) & 1;
}

static inline int _shiftDist(int bit){
	return 8 - 1 - bit % 8;
}

static void _printBinaryBytes(const Byte_t *bytes, int numBytes){
	int bit;
	for(bit = 0; bit < numBytes * 8; bit++){
		printf("%-3d", bit);
	}
	puts("");

	for(bit = 0; bit < numBytes * 8; bit++){
		printf("%-3d", _getBit(bytes, bit));
		// printf("%d", _getBit(bytes, bit));
	}
	puts("");
}
