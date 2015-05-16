#include <stdio.h>
#include <stdbool.h>

#include "sieve_of_eratosthenes.h"

void test_findNthPrimeNumber(void){
	puts("Testing findNthPrimeNumber() against the first 100 primes.");
	const int numPrimes = 100;
	const int primes[] = {
		2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
		71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139,
		149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
		227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293,
		307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383,
		389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
		467, 479, 487, 491, 499, 503, 509, 521, 523, 541
	};

	bool allPass = true;
	for(int ind = 0; ind < numPrimes; ind++){
		int primeNumber = ind + 1;
		int actual = findNthPrimeNumber(primeNumber);
		int expected = primes[ind];
		if(actual != expected){
			fprintf(
				stderr, "prime %d: actual %d != expected %d.\n",
				primeNumber, actual, expected
			);
			allPass = false;
		}
	}

	if(allPass){
		puts("All passed!");
	}
	else {
		fputs("ERROR: One or more tests failed!\n", stderr);
	}
}

int main(){
	test_findNthPrimeNumber();
	return 0;
}
