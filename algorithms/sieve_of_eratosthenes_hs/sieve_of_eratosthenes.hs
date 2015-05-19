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

mergeInfinite :: (Ord a) => [a] -> [a] -> [a]
mergeInfinite as@(a:aTail) bs@(b:bTail)
	| a < b = a : mergeInfinite aTail bs
	| b < a = b : mergeInfinite as bTail
	| otherwise = a : mergeInfinite aTail bTail
mergeInfinite _ _ = error "Impossible."

diffInfinite :: (Ord a) => [a] -> [a] -> [a]
diffInfinite as@(a:aTail) bs@(b:bTail)
	| a < b = a : diffInfinite (aTail) bs
	| a > b = diffInfinite as bTail
	| otherwise = diffInfinite aTail bTail
diffInfinite _ _ = error "Impossible."

primeNumbers', nonPrimes :: [Int]
primeNumbers' = 2 : 3 : 5 : diffInfinite [7, 9..] nonPrimes
nonPrimes = foldr1 mergeAll $ map generateMultiples $ tail primeNumbers'
	where
		mergeAll (x:xt) multiples = x : mergeInfinite xt multiples
		generateMultiples prime = map (\ num -> (prime + num) * prime) [0, 2..]
