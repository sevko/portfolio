module Rsa where

import qualified ModularExponentiation as ModularExp
import qualified ExtendedEuclideanAlgorithm as ExtEuclidean

type PublicKey = Integer
type PrivateKey = Integer
type Modulus = Integer

decrypt :: PrivateKey -> Modulus -> Integer -> Integer
decrypt modulus privateKey ciphertext =
	ModularExp.modularPow''' ciphertext privateKey modulus

encrypt :: PublicKey -> Modulus -> Integer -> Integer
encrypt modulus publicKey plaintext =
	ModularExp.modularPow''' plaintext publicKey modulus

create :: Integer -> Integer -> (Modulus, PublicKey, PrivateKey)
create p q = let
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
