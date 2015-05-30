{-
 - Tests for the `ExtEuclidean` module.
 -}

import qualified System.Random as Random
import qualified Text.Format as Format
import qualified Data.List.Split as Split
import qualified ExtendedEuclideanAlgorithm as ExtEuclidean

{-
 - Test the output of `ExtEuclidean.bezoutCoefficients` against a
 - large number of randomized test-cases.
 -}
testBezoutCoefficients :: IO ()
testBezoutCoefficients = do
	let numberOfRandomTests = 1000

	randomGen <- Random.getStdGen
	let
		testCases = map (\ [a, b] -> (a, b)) $
			Split.chunksOf 2 $
			take (numberOfRandomTests * 2) $
			Random.randomRs (-100, 100) randomGen

		testResults = map (\ (a, b) ->
			let
				(coef1, coef2, _) =
					ExtEuclidean.bezoutCoefficients a b
				actualGcd = coef1 * a + coef2 * b
				expectedGcd = gcd a b
				result = actualGcd == expectedGcd
			in (a, b, actualGcd, expectedGcd, result))
			testCases

		(numPass, numFail) = foldr
			(\ (_, _, _, _, pass) (numPass', numFail') ->
				if pass
					then (numPass' + 1, numFail')
					else (numPass', numFail' + 1))
			(0, 0)
			testResults

	-- Print out a message for each failure.
	mapM_ (\ (num1, num2, actual, expected, pass) ->
		if pass
			then return ()
			else putStrLn $ Format.format
				"Failed for ({0}, {1}): expecting a computed gcd of {2} but \
				\the result was {3}."
				[show num1, show num2, show expected, show actual])
		testResults

	putStrLn "Testing `bezoutCoefficients`"
	putStrLn $ "Number of tests: " ++ show numberOfRandomTests ++ ". Results:"
	putStrLn $ "pass: " ++ show numPass
	putStrLn $ "fail: " ++ show numFail

	if numFail > 0
		then putStrLn "ERROR: One or more failures!"
		else putStrLn "All tests passed!"

{-
 - Test the output of `ExtEuclidean.modularInverse` against a
 - large number of randomized test-cases.
 -}
testModularInverse :: IO ()
testModularInverse = do
	randomGen <- Random.getStdGen
	let
		numberOfRandomTests = 1000
		testCases =
			Split.chunksOf 2 $
			take (numberOfRandomTests * 2) $
			Random.randomRs (-100, 100) randomGen

		evalTestCase [num, modulus] = do
			let
				inverse = ExtEuclidean.modularInverse num modulus
				(_, _, gcd') = ExtEuclidean.bezoutCoefficients num modulus
			if gcd' == 1
				then
					let
						(Just inverseVal) = inverse
						modProduct = inverseVal * num `mod` modulus

						-- This'll equal 1 UNLESS `modulus` is negative, in
						-- which case it'll become `modulus - 1`.
						expectedGcd = 1 `mod` modulus
					in if modProduct == expectedGcd
					then return ()
					else putStrLn $ Format.format
						"Failed for {0} modulo {1}! Got {2}, but \
						\`({2} * {0}) mod {1}` equals {3}, not {4}." $
						map show
						[num, modulus, inverseVal, modProduct, expectedGcd]

				else if inverse == Nothing
					then return ()
					else putStrLn $ Format.format
						"Failed for {0} modulo {1}! Expecting `Nothing` \
						\because gcd({0}, {1}) != 1, but got {2}" $
						[show num, show modulus, show inverse]
		evalTestCase _ = error "`testCases` has an incorrect number of \
			\values per test-case."

	putStrLn "Testing `modularInverse`"
	mapM_ evalTestCase testCases

main :: IO ()
main = do
	testBezoutCoefficients
	testModularInverse
