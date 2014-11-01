# DES
An implementation of the <b>D</b>ata <b>E</b>ncryption <b>S</b>tandard (**DES**), for encrypting and decrypting data.
The DES module exports two functions, `DES_encipher()` and `DES_decipher()`, as well as a `typedef`'d data-type
`Byte_t` (as inherited from the `BitOps` library, also implemented in this repo). Read the in-code (`src/des.h`) API
documentation for more detailed information. Sample usage:

```c
#include "des.h"

int main(){
	Byte_t plaintext[] = {'d', 'e', 'a', 'd', 'b', 'e', 'e', 'f'};
	Byte_t key[] = {'1', 'B', 'A', 'D', 'B', '0', '0', '2'};

	Byte_t *ciphertext = DES_encipher(plaintext, key);
	Byte_t *plaintext2 = DES_encipher(ciphertext, key);

	return 0;
}
```

The module has `libtap` unit-tests; to run them:

```bash
make test
make run
```

Pipe `make run` through something like `tap-spec` for human-readable output, unless you're a robot.
