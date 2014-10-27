#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "bit_ops.h"

// extern inline void BitOps_setBit(Byte_t *bytes, int bitPos);
extern inline int BitOps_getBit(const Byte_t *bytes, int bitPos);
// extern inline void BitOps_rotLeft(Byte_t *bytes, int numBits);

char *BitOps_getBitString(Byte_t *bytes, int numBytes){
	int numBits = numBytes * CHAR_BIT;
	char *bitString = malloc(numBits + 1);
	int bit;
	for(bit = 0; bit < numBits; bit++){
		bitString[bit] = '0' + BitOps_getBit(bytes, bit);
	}
	bitString[bit] = 0;
	return bitString;
}
