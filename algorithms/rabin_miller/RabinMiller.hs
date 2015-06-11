{-
 - An implementation of Rabin Miller.
 -}

module RabinMiller where

import qualified ModularExponentiation as ModExp
import qualified System.Random as Random

{-
 - Test `num` for primality using the probabilistic Rabin-Miller algorithm.
 - Note that it needs to randomly generate several numbers to do so (hence the
 - `IO`-wrapped return value); for a function that implements the same
 - algorithm, but accepts said numbers as an argument, see `isPrimeCore` (for
 - which this function is actually just a wrapper).
 -}
isPrime :: Integer -> IO Bool
isPrime primeCandidate = do
	randomGen <- Random.getStdGen
	let
		numberTests = 5
		testNumbers = take numberTests $
			(Random.randomRs (2, primeCandidate - 1) randomGen)

	return $ isPrimeCore primeCandidate testNumbers

{-
 - Test `num` for primality with the probabilistic Rabin-Miller algorithm,
 - using `testNumbers` as the list of test-cases (potential "witnesses" for the
 - non-primality of `num`). It's unlikely that you'll ever want to specify
 - these on your own, so see `isPrime`.
 -}
isPrimeCore :: Integer -> [Integer] -> Bool
isPrimeCore 2 _ = True
isPrimeCore num testNumbers
	| num `mod` 2 == 0 || num <= 1 = False
	| otherwise = all isWitness testNumbers
	where
		(powerOf2, otherFactor) = decomposeToFactorsOf2 num

		isWitness :: Integer -> Bool
		isWitness testNum
			| baseNumber == 1 || baseNumber == num + 1 = True
			| otherwise =
				let squares = map ((baseNumber ^) . (2 ^)) [0..powerOf2 - 1]
				in any (\ power -> power `mod` num == num - 1) squares
			where
				baseNumber = ModExp.modularPow testNum otherFactor num

{-
 - Decompose `num` into the form `(2 ^ m) * n`, returning (m, n).
 -}
decomposeToFactorsOf2 num =
	let (quotient, remainder) = divMod num 2
	in if remainder == 0
		then let (power, otherFactor) = decomposeToFactorsOf2 quotient
			in (power + 1, otherFactor)
		else (1, quotient)
