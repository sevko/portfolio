type SieveEntry = (Bool, Int)
type Sieve = [SieveEntry]

upperBoundNthPrime :: Int -> Int
upperBoundNthPrime n
	| n >= 7022 = ceiling $ n' * (log n' + log (log n') - 0.9385)
	| n >= 6 = ceiling $ n' * (log n' + log (log n'))
	| otherwise = [3, 4, 6, 8, 12] !! (n - 1)
	where n' = fromIntegral n

checkOffFactors :: Sieve -> Int -> Sieve
checkOffFactors sieve factor =
	map (\ entry@(isPrime, num) ->
		if num <= factor
			then entry
			else (isPrime && num `mod` factor /= 0, num))
	sieve

findNthPrime :: Int -> Int
findNthPrime n = let
	upperBound = upperBoundNthPrime n
	numbers = zip (repeat True) [1..upperBound]
	factorUpperBound = ceiling $ sqrt $ fromIntegral upperBound
	sieve = foldl checkOffFactors numbers [2..factorUpperBound]
	in findNthPrimeInSieve n sieve
	where

		findNthPrimeInSieve :: Int -> Sieve -> Int
		findNthPrimeInSieve _ [] = error "nth prime not found in number search-space. Implementation error."
		findNthPrimeInSieve 0 ((True, num):_) = num
		findNthPrimeInSieve n' ((True, _):sieve) = findNthPrimeInSieve (n' - 1) sieve
		findNthPrimeInSieve n' ((False, _):sieve) = findNthPrimeInSieve n' sieve

main :: IO ()
main = do
	print $ findNthPrime 10000
