#include <stdio.h>
#include <stdlib.h>

#include "bit_ops.h"
#include "src/des.h"

int main(){
	Byte_t key[] = {0x13, 0x34, 0x57, 0x79, 0x9b, 0xbc, 0xdf, 0xf1};
	DES_encipher((const Byte_t *)"Hello, world.", key);
	return EXIT_SUCCESS;
}
