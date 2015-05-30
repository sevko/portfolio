{-
 - Tests for the `ModularExponentiation` functions.
 -}

import qualified ModularExponentiation as ModExp
import qualified Text.Format as Format

{-
 - Test the output of the various functions provided by `ModularExponentiation`
 - against a hardcoded input/expected output.
 -}
testFunctions :: String
testFunctions =
	let
		base = 423123213123123123123131
		exp' = 1000000 :: Integer
		mod' = 497939485920
		funcs = [
			("modularPow' (tail-recursive)", ModExp.modularPow'),
			("modularPow'' (strict foldl')", ModExp.modularPow''),
			("modularPow''' (binary exponentiation)", ModExp.modularPow''')]
		results = map (\ (name, func) -> (name, func base exp' mod')) funcs
		expected = 67540154401
		resultStrings = map(\ (name, actual) ->
			let
				testResult = if actual == expected
					then "pass"
					else Format.format
						"fail, expecting {0} but got {1}"
						[show expected, show actual]
			in Format.format "{0}: {1}" [name, testResult]) results
	in unlines resultStrings

main :: IO ()
main = do
	putStrLn "Running tests:"
	putStr testFunctions
