{-
An implementation of the Sieve of Eratosthenes, using this article:
http://en.literateprograms.org/Sieve_of_Eratosthenes_%28Haskell%29 as a
tutorial.
-}

module SieveOfEratosthenes where

{- A naive, slow, but elegant implementation. -}
primeNumbers :: [Int]
primeNumbers = 2 : sieve [3, 5..]
	where
		sieve (factor:primes) =
			let sansMultiples = filter (\ num -> num `mod` factor /= 0) primes
			in factor : sieve sansMultiples
		sieve _ = error "Implementation error."

{- A significantly faster but infinitely more complex version. -}
primeNumbers' :: [Int]
primeNumbers' = 2 : 3 : 5 : diffInfinite [7, 9..] nonPrimes
	where
		nonPrimes = foldr1 mergeAll $ map generateMultiples $ tail primeNumbers'

		mergeAll :: [Int] -> [Int] -> [Int]
		mergeAll (x:xt) multiples = x : mergeInfinite xt multiples
		mergeAll _ _ = error "Impossible, the list is infinite."

		generateMultiples :: Int -> [Int]
		generateMultiples prime = map (\ num -> (prime + num) * prime) [0, 2..]

		{-
		 - Remove all of the elements in the second infinite list from the
		 - first.
		 -}
		diffInfinite :: (Ord a) => [a] -> [a] -> [a]
		diffInfinite as@(a:aTail) bs@(b:bTail)
			| a < b = a : diffInfinite (aTail) bs
			| a > b = diffInfinite as bTail
			| otherwise = diffInfinite aTail bTail
		diffInfinite _ _ = error "The lists were not infinite!"

		{- Merge two infinite ordered lists into one ordered list. -}
		mergeInfinite :: (Ord a) => [a] -> [a] -> [a]
		mergeInfinite as@(a:aTail) bs@(b:bTail)
			| a < b = a : mergeInfinite aTail bs
			| b < a = b : mergeInfinite as bTail
			| otherwise = a : mergeInfinite aTail bTail
		mergeInfinite _ _ = error "The lists were not infinite!"
