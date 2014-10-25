#include <stdio.h>
#include <stdlib.h>

#include "src/des.h"

int main(){
	DES_encipher((const Byte_t *)"Hello, world.", (Byte_t *)"super secret key");
	return EXIT_SUCCESS;
}
