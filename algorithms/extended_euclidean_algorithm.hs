{-
 - An implementation of the extended Euclidean algorithm.
 -}

import qualified System.Random as Random
import qualified Text.Format as Format
import qualified Data.List.Split as Split

{-
 - An implementation of the Extended Euclidean Algorithm, which, given two
 - integers `a` and `b`, computes the coefficients `x` and `y` that satisfy the
 - Bezout identity: `a * x + b * y = gcd(a, b)`. Both positive and negative
 - values are accepted.
 -}
bezoutCoefficients :: Int -> Int -> (Int, Int)

{-
 - Given `b = 0`, we know that `gcd(a, b)` is `abs(a)`, and also that, in the
 - Bezout identity (`a * x + b * y = gcd(a, b)`), `x` must equal 1 if `a` is
 - positive and -1 if `a` is negative. `b` can be anything, so we'll just
 - choose 0.
 -}
bezoutCoefficients a 0 = (if a < 0 then -1 else 1, 0)
bezoutCoefficients a b = let
	(quotient, remainder) = divMod a b
	(firstCoef, secondCoef) = bezoutCoefficients b remainder
	in (secondCoef, firstCoef - quotient * secondCoef)

{-
 - Test the output of `bezoutCoefficients` against a large number of randomized
 - test-cases.
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

		testResults = map (\ testNumbers@(a, b) ->
			let
				(coef1, coef2) = bezoutCoefficients a b
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

	putStrLn $ "Number of tests: " ++ show numberOfRandomTests ++ ". Results:"
	putStrLn $ "pass: " ++ show numPass
	putStrLn $ "fail: " ++ show numFail

	if numFail > 0
		then error "ERROR: One or more failures!"
		else putStrLn "All tests passed!"

main :: IO ()
main = testBezoutCoefficients
