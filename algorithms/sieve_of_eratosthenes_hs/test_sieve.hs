{-
Functions to test the SieveOfEratosthenes module.
-}

import qualified Text.Format as Format
import qualified SieveOfEratosthenes

main :: IO ()
main = do
	let first100Primes = [
		2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
		71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139,
		149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
		227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293,
		307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383,
		389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
		467, 479, 487, 491, 499, 503, 509, 521, 523, 541
		]
	let
		reviewTestResult (id', actual, expected) =
			putStrLn $ Format.format "Prime #{0}: {1}" [
				show id',
				if actual == expected
					then "success: expected == actual == " ++ show expected
					else Format.format
						"failure: expected ({0}) != actual ({1})"
						[show expected, show actual]
				]

		actualVsExpected = zip3
			[1..]
			(take 100 SieveOfEratosthenes.primeNumbers)
			first100Primes
		numberFailed = length $
			filter (\ (_, actual, expected) -> actual /= expected)
			actualVsExpected

	mapM_ reviewTestResult actualVsExpected
	if numberFailed > 0
		then error "One or more failures!"
		else putStrLn "All passed!"
