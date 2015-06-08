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
isWitness num powerOf2 otherFactor testNum
	| baseNumber == 1 || baseNumber == num + 1 = True
	| otherwise =
		let powersOf2 = scanl (\ prevSquare _ -> prevSquare ^ 2) baseNumber
			[1..powerOf2 - 1]
		in any (\ power -> power `mod` num == num - 1) powersOf2
	where baseNumber = ModExp.modularPow testNum otherFactor num

decomposeToFactorsOf2 num =
	let (quotient, remainder) = divMod num 2
	in if remainder == 0
		then let (power, otherFactor) = decomposeToFactorsOf2 quotient
			in (power + 1, otherFactor)
		else (1, quotient)
