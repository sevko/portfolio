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
	#define NUM_TO_IND(num) ((num - 3) / 2)
	#define IND_TO_NUM(num) (num * 2 + 3)

	if(n == 1){
		return 2;
	}

	const int upperBound = nthPrimeUpperBound(n);
	const int numNumbers = NUM_TO_IND(upperBound) + 1;
	char numbers[numNumbers];
	memset(numbers, 0, numNumbers);

	int upperFactorBound = ceil(sqrt(upperBound));
	int factor = 3;
	while(factor <= upperFactorBound){
		int multiple = factor * 3;
		while(multiple < upperBound){
			numbers[NUM_TO_IND(multiple)] = 1;
			multiple += factor * 2;
		}

		int factorInd = NUM_TO_IND(factor);
		while(numbers[++factorInd] == 1);
		factor = IND_TO_NUM(factorInd);
	}

	int numPrimes = 1;
	for(int ind = 0; ind < numNumbers; ind++){
		if(numbers[ind] == 0 && ++numPrimes == n){
			return IND_TO_NUM(ind);
		}
	}
	return -1;
}
