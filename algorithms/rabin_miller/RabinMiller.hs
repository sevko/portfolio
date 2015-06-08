module RabinMiller where

import qualified ModularExponentiation as ModExp
import qualified System.Random as Random

isPrime :: Integer -> IO Bool
isPrime primeCandidate = do
	randomGen <- Random.getStdGen
	let
		numberTests = 5
		testNumbers = take numberTests $
			(Random.randomRs (2, primeCandidate - 1) randomGen)

	return $ isPrimeCore primeCandidate testNumbers

isPrimeCore :: Integer -> [Integer] -> Bool
isPrimeCore 2 _ = True
isPrimeCore num testNumbers
	| num `mod` 2 == 0 || num <= 1 = False
	| otherwise =
		let (powerOf2, otherFactor) = decomposeToFactorsOf2 num
		in all (isWitness num powerOf2 otherFactor) testNumbers

isWitness :: Integer -> Integer -> Integer -> Integer -> Bool
isWitness primeCandidate powerOf2 otherFactor testNum = let
	baseNumber = ModExp.modularPow testNum otherFactor primeCandidate
	in if baseNumber == 1 || baseNumber == primeCandidate + 1
		then True
		else let
			powersOf2 = scanl (\ prevSquare _ -> prevSquare ^ 2) baseNumber
				[1..powerOf2 - 1]
			in any
				(\ num -> num `mod` primeCandidate == primeCandidate - 1)
				powersOf2

decomposeToFactorsOf2 num =
	let (quotient, remainder) = divMod num 2
	in if remainder == 0
		then let (power, otherFactor) = decomposeToFactorsOf2 quotient
			in (power + 1, otherFactor)
		else (1, quotient)
