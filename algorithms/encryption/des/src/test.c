#include <stdio.h>
#include <stdlib.h>

#include "src/des.h"

int main(){
	DES_encipher("Hello, world.", "super secret key");
	return EXIT_SUCCESS;
}
