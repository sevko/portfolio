{-
 - An implementation of the RSA algorithm, which relies on implementations of
 - certain algorithms (like modular exponentiation, Rabin Miller primality
 - testing, modular inverse computation, etc.) found elsewhere in this repo.
 -}

module Rsa where

import qualified ModularExponentiation as ModularExp
import qualified ExtendedEuclideanAlgorithm as ExtEuclidean
import qualified RabinMiller
import qualified System.Random as Random

-- Types for readability.
type PublicKey = Integer
type PrivateKey = Integer
type Modulus = Integer

{-
 - Decrypt `ciphertext`.
 -}
decrypt :: PrivateKey -> Modulus -> Integer -> Integer
decrypt modulus privateKey ciphertext =
	ModularExp.modularPow''' ciphertext privateKey modulus

{-
 - Encrypt `plaintext`.
 -}
encrypt :: PublicKey -> Modulus -> Integer -> Integer
encrypt modulus publicKey plaintext =
	ModularExp.modularPow''' plaintext publicKey modulus

{-
 - The "pure" key creation function, which accepts the two primes necessary to
 - generate the public and private keys as arguments. Also see `create`, which
 - handles generating random primes for you.
 -}
createCore :: Integer -> Integer -> (Modulus, PublicKey, PrivateKey)
createCore p q = let
	n = p * q
	totient = (p - 1) * (q - 1)

	findPublicAndPrivateKeys :: [Integer] -> (PublicKey, PrivateKey)
	findPublicAndPrivateKeys (currCandidate:candidates) =
		let (modularInverse, _, gcd') =
			ExtEuclidean.bezoutCoefficients currCandidate totient
		in if gcd' == 1
			then (currCandidate, modularInverse)
			else findPublicAndPrivateKeys candidates

	findPublicAndPrivateKeys [] = error $
		"No public key `e` could be generated such that `gcd(e, t) = 1`, \
		\where `t` is the totient (in this case, " ++ show totient ++ ")."

	eCandidates = [3..totient - 1]
	(e, d) = findPublicAndPrivateKeys eCandidates
	d' = if d < 0 then d + totient else d
	in (n, e, d')

{-
 - Create an RSA key-pair using two randomly generated primes (hence the `IO`
 - wrapped return values.)
 -}
create :: Int -> IO (Modulus, PublicKey, PrivateKey)
create numBits = do
	p <- genRandomPrime numBits
	q <- genRandomPrime numBits
	return $ createCore p q

{-
 - Find a random prime number of a `bitCount` bit-length. Note that the
 - algorithm is pretty simple: it simply selects a random number of length
 - `bitCount`, and, iterating through all of the odd numbers greater than it,
 - returns the first one that's identified as prime via a Rabin-Miller
 - primality test. This might actually result in the creation of a prime of
 - a bit length GREATER than `bitCount`, however unlikely, if the base number
 - is near the upper bound for values of length `bitCount` and iterating over
 - larger numbers takes us into greater bit-length territory before we manage
 - to find a prime. Some optimizations might also be possible, like jumping to
 - another random base number after several iterations off the current one
 - don't yield a prime (reportedly due to the intermittenly large gaps between
 - primes, which we might get stuck in by sticking to only one base number).
 -}
genRandomPrime :: Int -> IO Integer
genRandomPrime bitCount = do
	randomGen <- Random.getStdGen
	let
		lowerBound = (2 ^ (bitCount - 2))
		upperBound = (2 ^ (bitCount - 1) - 1)
		(baseNum, _) = Random.randomR (lowerBound, upperBound) randomGen

	iterateToPrime (if even baseNum then baseNum + 1 else baseNum)
	where
		iterateToPrime num = do
			isPrime <- RabinMiller.isPrime num
			if isPrime
				then return num
				else iterateToPrime $ num + 2
