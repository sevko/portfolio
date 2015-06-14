{-
 - Tests for the RabinMiller module.
 -}

import qualified RabinMiller
import qualified Text.Format as Format

main :: IO ()
main = do

	-- Test the output of `isPrime` against the values in the range of the
	-- first 100 primes; anything not in `first100primes` should be identified
	-- as composite, and everything in it as prime.
	let
		first100primes = [
			2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61,
			67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137,
			139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199,
			211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277,
			281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359,
			367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439,
			443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521,
			523, 541]

	testResults <- mapM (\ num -> do
		let isPrime = num `elem` first100primes
		isPrimeTest <- RabinMiller.isPrime num
		return (isPrime == isPrimeTest, isPrime, isPrimeTest, num))
		[1..last first100primes]

	let
		(numPass, numFail) = foldl
			(\ (succ', fail') (passed, _, _, _) ->
				if passed then (succ' + 1, fail') else (succ', fail' + 1))
			(0, 0)
			testResults

		reportResult (passed, isPrime, isPrimeTest, num) = do
			if passed
				then return ()
				else putStrLn $ Format.format
					"Failed for {0}. Expecting {1}, but got {2}."
					[show num, show isPrime, show isPrimeTest]

	putStrLn "Running tests. Only failures will be reported."
	putStrLn $ Format.format "{0} tests: {1} passed, {2} failed." $
		map show [numPass + numFail, numPass, numFail]
	mapM_ reportResult testResults
