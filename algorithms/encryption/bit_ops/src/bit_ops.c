#include <stdio.h>
#include <limits.h>

#include "bit_ops.h"

// extern inline void BitOps_setBit(Byte_t *bytes, int bitPos);
extern inline int BitOps_getBit(const Byte_t *bytes, int bitPos);
// extern inline void BitOps_rotLeft(Byte_t *bytes, int numBits);

void BitOps_printBits(Byte_t *bytes, int numBytes){
	for(int bit = 0; bit < numBytes * CHAR_BIT; bit++){
		printf("%d", BitOps_getBit(bytes, bit));
	}
	puts("");
}
