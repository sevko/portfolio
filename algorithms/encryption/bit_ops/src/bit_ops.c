#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "bit_ops.h"

extern inline void BitOps_setBit(Byte_t *bytes, int bitPos);
extern inline int BitOps_getBit(const Byte_t *bytes, int bitPos);

void BitOps_rotLeft(Byte_t *bytes, int numBytes){
	int firstBit = BitOps_getBit(bytes, 0);
	bytes[0] <<= 1;
	for(int byte = 1; byte < numBytes; byte++){
		int firstBitPos = byte * CHAR_BIT;
		if(BitOps_getBit(bytes, firstBitPos)){
			BitOps_setBit(bytes, firstBitPos - 1);
		}
		bytes[byte] <<= 1;
	}

	if(firstBit){
		BitOps_setBit(bytes, numBytes * CHAR_BIT - 1);
	}
}

char *BitOps_getBitString(const Byte_t *bytes, int numBytes){
	int numBits = numBytes * CHAR_BIT;
	char *bitString = malloc(numBits + 1);
	int bit;
	for(bit = 0; bit < numBits; bit++){
		bitString[bit] = '0' + BitOps_getBit(bytes, bit);
	}
	bitString[bit] = 0;
	return bitString;
}
