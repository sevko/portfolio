 /**
 * @brief An implementation of common bit operations.
 */

#pragma once

#include <limits.h>

typedef unsigned char Byte_t;

/**
 * @brief Retrieve the value of a bit in an array of bytes.
 * @param bytes One or more bytes.
 * @param bitPos The bit position (counting from left-to-right, starting at 0)
 *      of the bit whose value will be retrieved. Must be within the inclusive
 *      range `[0, CHAR_BIT * len(bytes) - 1]`.
 * @return The value of the bit in `bytes` at index `bitPos`; either 0 or 1.
 */
inline int BitOps_getBit(const Byte_t *bytes, int bitPos){
	int shiftDist = CHAR_BIT - 1 - bitPos % CHAR_BIT;
	return (bytes[bitPos / CHAR_BIT] >> shiftDist) & 1;
}

/**
 * @brief Print an array of bytes in binary format (ie, the value of each bit).
 * @param bytes An array of one or more bytes.
 * @param numBytes The number of bytes in `bytes`.
 */
void BitOps_printBits(Byte_t *bytes, int numBytes);
