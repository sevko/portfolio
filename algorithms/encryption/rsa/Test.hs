{-
 - Unit tests for the functions provided by the `Rsa` module.
 -}

import qualified Rsa
import qualified Text.Format as Format

testEncrypt :: IO ()
testEncrypt = do
	putStrLn "Testing `Rsa.encrypt`."
	let testCases = [
		-- plaintext, public key, modulus, expected ciphertext
		(65, 17, 3233, 2790),
		(2, 7, 143, 128),
		(100, 5, 299, 16)]
	let evalTestCase (plaintext, publicKey, modulus, expectedCiphertext) =
		let actualCiphertext = Rsa.encrypt modulus publicKey plaintext
		in if actualCiphertext == expectedCiphertext
			then return ()
			else putStrLn $ Format.format
				"Failed to encrypt {0} with public key \
				\{1} and modulus {2}. Got {3}, but expected {4}." $
				map show [
					plaintext, publicKey, modulus, actualCiphertext,
					expectedCiphertext]
	mapM_ evalTestCase testCases

testDecrypt :: IO ()
testDecrypt = do
	putStrLn "Testing `Rsa.decrypt`."
	let testCases = [
		(2790, 2753, 3233, 65),
		(128, 103, 143, 2),
		(16, 53, 299, 100)]

	let evalTestCase (ciphertext, privateKey, modulus, expectedPlaintext) = do
		let actualPlaintext = Rsa.decrypt modulus privateKey ciphertext
		if actualPlaintext == expectedPlaintext
			then return ()
			else putStrLn $ Format.format
				"Failed to decrypt {0} with private key {1} \
				\and modulus {2}. Got {3}, but expected {4}." $
				map show [
					ciphertext, privateKey, modulus, actualPlaintext,
					expectedPlaintext]

	mapM_ evalTestCase testCases

testCreate :: IO ()
testCreate = do
	putStrLn "Testing `Rsa.create`."
	let testCases = [
		(61, 53, (3233, 7, 1783)),
		(11, 13, (143, 7, 103)),
		(13, 23, (299, 5, 53))]

	let evalTestCase (prime1, prime2, expectedKeys) = do
		let actualKeys = Rsa.createCore prime1 prime2
		if actualKeys == expectedKeys
			then return ()
			else putStrLn $ Format.format
				"Failed to create keys for primes {0} and {1}. Got \
				\{2} but expected {3}."
				[show prime1, show prime2, show actualKeys, show expectedKeys]

	mapM_ evalTestCase testCases

main :: IO ()
main = do
	testEncrypt
	testDecrypt
	testCreate
