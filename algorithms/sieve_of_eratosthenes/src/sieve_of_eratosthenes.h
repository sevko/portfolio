#pragma once

/*
 * Find and return the `n`th prime number using the Sieve of Eratosthenes. If
 * the prime couldn't be computed (likely an implementation error), 0 will be
 * returned.
 *
 * Complexity:
 *   space: n * log(n)
 *   time: (n * log(n)) ^ (3 / 2)
 */
unsigned int findNthPrimeNumber(int n);
