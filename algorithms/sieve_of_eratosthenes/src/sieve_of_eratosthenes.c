#include <stdio.h>
#include <string.h>
#include <math.h>

/* Return the upper bound for the `n`th prime. */
static unsigned int nthPrimeUpperBound(int n){
	if(n >= 7022){
		/*
		 * See page 2 of https://www.maa.org/sites/default/files/jaroma03200545640.pdf
		 */
		return n * (log(n) + log(log(n)) - 0.9385);
	}
	else if(n >= 6){
		/*
		 * See http://en.wikipedia.org/wiki/Prime_number_theorem#Approximations_for_the_nth_prime_number
		 * Since the inequality only holds for `n >= 6`, we proceed to hardcode
		 * the upper bounds for lower `n`.
		 */
		return n * (log(n) + log(log(n)));
	}
	else {
		static const int smallUpperBounds[] = {3, 4, 6, 8, 12};
		return smallUpperBounds[n - 1];
	}
}

unsigned int findNthPrimeNumber(int n){
	/**
	 * This prime finder uses a moderately optimized Sieve of Eratosthenes.
	 * The range of values is stored on the stack in a `numbers` array (note
	 * that this is dangerous, because we may very well exceed the maximum
	 * stack size for large enough `n`). Each index contains a binary value:
	 * whether the number corresponding to that index has been marked off as a
	 * multiple of a lesser number (1), or not (0). The `char` type is used to
	 * reduce the array's footprint, but bit masking would be ideal.. as far as
	 * space is concerned, anyway. Only odd values starting from 3 are stored
	 * in the array, since we don't need to bother checking even values for
	 * primality. Consequently, when we check off the multiples of each
	 * successive prime number, we only visit the odd ones.
	 */

	// Convenience macros for converting a number to a slot in the `numbers`
	// array.
	#define NUM_TO_IND(num) ((num - 3) / 2)
	#define IND_TO_NUM(num) (num * 2 + 3)

	// A hack necessary to account for the fact that the `numbers` array starts
	// storing values from 3 (which is necessary because of the assumption that
	// every value inside of it is odd), meaning that 2, which is the only even
	// prime, is simply never accounted for.
	if(n == 1){
		return 2;
	}

	const int upperBound = nthPrimeUpperBound(n);
	const int numNumbers = NUM_TO_IND(upperBound) + 1;
	char numbers[numNumbers];
	memset(numbers, 0, numNumbers);

	// Checking any factors above the square-root of the upper bound would be
	// redundant with the already visited factors below it.
	int upperFactorBound = ceil(sqrt(upperBound));

	int factor = 3;
	int factorInd = 0;
	while(factor <= upperFactorBound){
		int multiple = factor * 3;
		int multipleDelta = factor * 2;
		while(multiple < upperBound){
			numbers[NUM_TO_IND(multiple)] = 1;
			multiple += multipleDelta;
		}

		// Find the next prime number in `numbers`, and use that as the next
		// factor to check off.
		while(numbers[++factorInd] == 1);
		factor = IND_TO_NUM(factorInd);
	}

	int numPrimes = 1;
	for(int ind = 0; ind < numNumbers; ind++){
		if(numbers[ind] == 0 && ++numPrimes == n){
			return IND_TO_NUM(ind);
		}
	}

	return 0;
}
