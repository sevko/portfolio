/**
 * @brief An implementation of the DES encryption algorithm.
 */

#pragma once

typedef unsigned char Byte_t;

Byte_t *DES_encipher(const Byte_t *plaintext, Byte_t *key);
Byte_t *DES_decipher(const Byte_t *ciphertext, Byte_t *key);
