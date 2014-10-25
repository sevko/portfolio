#include <stdio.h>
#include <stdlib.h>

#include "des.h"

static inline void _setBit(Byte_t *bytes, int bit);
static inline int _getBit(const Byte_t *bytes, int bit);
static void _printBinaryBytes(const Byte_t *bytes, int numBytes);
static inline int _shiftDist(int bit);

Byte_t *DES_encipher(const Byte_t *plaintext, Byte_t *key){
	return (Byte_t *)plaintext;
}

static inline void _setBit(Byte_t *bytes, int bit){
	bytes[bit / 8] |= 1 << _shiftDist(bit);
}

static inline int _getBit(const Byte_t *bytes, int bit){
	return ((int)(bytes[bit / 8] >> _shiftDist(bit))) & 1;
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
	}
	puts("");
}
