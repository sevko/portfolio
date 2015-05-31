import qualified Rsa
import qualified Text.Format as Format

testRsa :: IO ()
testRsa = do
	let
		message = 65
		(modulus, publicKey, privateKey) = Rsa.create 61 53

	putStrLn $ Format.format
		"Using modulus {0}, public key {1}, and private key {2}."
		[show modulus, show publicKey, show privateKey]

	let
		encrypted = Rsa.encrypt modulus publicKey message
		decrypted = Rsa.decrypt modulus privateKey encrypted

	putStrLn $ Format.format
		"Message {0} is {1} when encrypted and {2} when decrypted."
		[show message, show encrypted, show decrypted]

main :: IO ()
main = testRsa
