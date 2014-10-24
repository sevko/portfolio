#include <stdio.h>
#include <stdlib.h>

#include "des.h"

Byte_t *DES_encipher(const Byte_t *plaintext, Byte_t *key){
	printf("Encrypted: %s\n", plaintext);
	return plaintext;
}
