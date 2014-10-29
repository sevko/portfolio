/**
 * @brief An implementation of the DES encryption algorithm.
 */

#pragma once

#include "bit_ops.h"

Byte_t *DES_encipher(const Byte_t *plaintext, const Byte_t *key);
Byte_t *DES_decipher(const Byte_t *ciphertext, const Byte_t *key);
