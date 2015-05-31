import qualified ModularExponentiation as ModularExp
import qualified ExtendedEuclideanAlgorithm as ExtEuclidean
import qualified Data.List as List

import qualified Text.Format as Format

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
	eCandidates = [3..totient - 1]
	e = case List.find (\ num -> totient `mod` num /= 0) eCandidates of
		Just prime -> prime
		Nothing -> error $
			"No prime `p` was found that satisfies the following\
				\constraints:\n\
				\  - 1 < p < totient\n\
				\  - gcd(p, totient) = 1 (ie, `p` and `totient` are coprime)\n\
				\for totient " ++ show totient ++ "."

	d = case ExtEuclidean.modularInverse e totient of
		Just inverse -> inverse
		Nothing -> error $ Format.format
			"e ({0}) is not coprime with the totient ({1})! This is an \
				\implementation error."
			[show e, show totient]
	d' = if d < 0 then d + totient else d
	in (n, e, d')

main :: IO ()
main = do
	let
		message = 65
		(modulus, publicKey, privateKey) = create 61 53

	putStrLn $ Format.format
		"Using modulus {0}, public key {1}, and private key {2}."
		[show modulus, show publicKey, show privateKey]

	let
		encrypted = encrypt modulus publicKey message
		decrypted = decrypt modulus privateKey encrypted

	putStrLn $ Format.format
		"Message {0} is {1} when encrypted and {2} when decrypted."
		[show message, show encrypted, show decrypted]
