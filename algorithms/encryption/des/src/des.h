/**
 * @brief An implementation of the DES encryption algorithm.
 */

#pragma once

#include "bit_ops.h"

Byte_t *DES_encipher(const Byte_t *plaintext, const Byte_t *key);
Byte_t *DES_decipher(const Byte_t *ciphertext, const Byte_t *key);

/**
 * Static function specifiers are discarded when this module is compiled in a
 * testing environment, exposing the corresponding functions for direct calls
 * from unit tests. Forward declarations for all such functions are
 * conditionally included; the documentation for each of them can be found
 * inside `des.c`.
 */
#ifndef DES_TEST
#define test_static static
#else
#define test_static
void _generateSubkeys(const Byte_t *key, Byte_t subkeys[][6]);
void _rotLeft(Byte_t *bytes, int rotDist);
#endif
