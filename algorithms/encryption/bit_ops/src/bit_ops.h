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
 * @brief Set the bit at a certain index of a byte array to 1.
 * @param bytes An array of one or more bytes.
 * @param bitPos The bit position (counting from left-to-right, starting at 0)
 *      of the bit whose value will be set. Must be within the inclusive
 *      range `[0, CHAR_BIT * len(bytes) - 1]`.
 */
inline void BitOps_setBit(Byte_t *bytes, int bitPos){
	int shiftDist = CHAR_BIT - 1 - bitPos % CHAR_BIT;
	bytes[bitPos / CHAR_BIT] |= 1 << shiftDist;
}

/**
 * @brief Left-rotate an array of bytes.
 * @param bytes An array of one or more bytes.
 * @param numBytes The number of bytes inside `numBytes`.
 */
void BitOps_rotLeft(Byte_t *bytes, int numBytes);

/**
 * @brief Returns a binary string representation of an array of bytes.
 * @param bytes An array of one or more bytes.
 * @param numBytes The number of bytes in `bytes`.
 * @return A null-terminated string containing a 0/1 for every bit in `bytes`.
 *      Must be deallocated by the caller.
 */
char *BitOps_getBitString(Byte_t *bytes, int numBytes);
