{-
 - An implementation of Euclid's algorithm.
 -}

import qualified Text.Format as Format
import qualified System.Random as Random
import qualified Data.List.Split as Split

{-
 - Return the greatest common denominator of a and b using Euclid's Algorithm.
 -}
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b $ a `rem` b

{-
 - Test `gcd'` against the `gcd` provided by `Prelude` for a large number of
 - randomly generated pairs of values.
 -}
testGcd' :: IO ()
testGcd' = do
	let numberOfRandomTests = 10000

	randomGen <- Random.getStdGen
	let
		testCases = map (\ [a, b] -> (a, b)) $
			Split.chunksOf 2 $
			take (numberOfRandomTests * 2) $
			Random.randomRs (-100, 100) randomGen

		testResults = map (\ (a, b) ->
			let
				actual = gcd a b
				expected = gcd' a b
			in (a, b, actual, expected, actual == expected))
			testCases

		(numPass, numFail) = foldr
			(\ (_, _, _, _, pass) (numPass', numFail') ->
				if pass
					then (numPass' + 1, numFail')
					else (numPass', numFail' + 1))
			(0, 0)
			testResults

	-- Print out a message for each failure.
	mapM_ (\ (num1, num2, expected, actual, pass) ->
		if pass
			then return ()
			else putStrLn $ Format.format
				"Failed for ({0}, {1}): expecting {2} but result was {3}."
				[show num1, show num2, show expected, show actual])
		testResults

	putStrLn $ "Number of tests: " ++ show numberOfRandomTests ++ ". Results:"
	putStrLn $ "pass: " ++ show numPass
	putStrLn $ "fail: " ++ show numFail

	if numFail > 0
		then error "ERROR: One or more failures!"
		else putStrLn "All tests passed!"

main :: IO ()
main = do
	testGcd'
