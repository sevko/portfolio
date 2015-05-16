#include <stdio.h>
#include <string.h>
#include <math.h>

/* Return the upper bound for the `n`th prime. */
static int nthPrimeUpperBound(int n){
	/*
	 * See here for citations for relevant academic material:
	 * http://en.wikipedia.org/wiki/Prime_number_theorem#Approximations_for_the_nth_prime_number
	 * Since the inequality only holds for `n >= 6`, we hardcode the upper
	 * bounds for lower `n`.
	 */
	if(n >= 6){
		return n * (log(n) + log(log(n)));
	}
	else {
		static const int smallUpperBounds[] = {3, 4, 6, 8, 12};
		return smallUpperBounds[n - 1];
	}
}

int findNthPrimeNumber(int n){
	const int upperBound = nthPrimeUpperBound(n);
	char numbers[upperBound];
	memset(numbers, 0, upperBound);

	int upperFactorBound = ceil(sqrt(upperBound));
	for(int factor = 2; factor <= upperFactorBound; factor++){
		int multiple = factor * 2;
		while(multiple < upperBound){
			numbers[multiple] = 1;
			multiple += factor;
		}
	}

	int numPrimes = 0;
	for(int ind = 2; ind < upperBound; ind++){
		if(numbers[ind] == 0 && ++numPrimes == n){
			return ind;
		}
	}
	return -1;
}
