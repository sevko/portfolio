{-
An implementation of the Sieve of Eratosthenes, using this article:
http://en.literateprograms.org/Sieve_of_Eratosthenes_%28Haskell%29 as a
tutorial.
-}

module SieveOfEratosthenes where

primeNumbers :: [Int]
primeNumbers = 2 : sieve [3, 5..]
	where
		sieve (factor:primes) =
			let sansMultiples = filter (\ num -> num `mod` factor /= 0) primes
			in factor : sieve sansMultiples
		sieve _ = error "Implementation error."
