"""
A self-contained implementation of the RSA encryption algorithm.
"""

import random

def bezout_coefficients(a, b):
	"""
	Return `(x, y)`, where `x` and `y` are the Bezout Coefficients of `a` and
	`b`, and thus satisfy: `a * x + b * y = gcd(a, b)`.
	"""

	if b == 0:
		return -1 if a < 0 else 1, 0, abs(a)
	else:
		quotient, remainder = divmod(a, b)
		coef1, coef2, gcd = bezout_coefficients(b, remainder)
		return coef2, coef1 - quotient * coef2, gcd

def modular_inverse(num, modulus):
	"""
	Return the modular inverse of `num` modulo `modulus`, or a value `n` that
	satisfies: `(num * n) % modulus = 1`
	"""

	coef1, _, gcd = bezout_coefficients(num, modulus)
	return coef1 if gcd == 1 else None

def gcd(a, b):
	"""
	Return the Greatest Common Divisor of `a` and `b`.
	"""

	return abs(a) if b == 0 else gcd(b, a % b)

def coprime(a, b):
	"""
	Return a boolean indicating whether `a` and `b` are coprime.
	"""

	return gcd(a, b) == 1

def create_key_pair(bit_length):
	"""
	Create and return an RSA key-pair of length `bit_length` in the form
	`(e, d, n)`, where `e` is the public key, `d` is the private key, and `n`
	is the modulus.
	"""

	prime_bit_length = bit_length // 2
	p = get_random_prime(prime_bit_length)
	q = get_random_prime(prime_bit_length)
	n = p * q
	totient = (p - 1) * (q - 1)

	while True:
		e_candidate = random.randint(3, totient - 1)
		if e_candidate % 2 == 0:
			e_candidate += 1

		if coprime(e_candidate, totient):
			e = e_candidate
			break

	d = modular_inverse(e, totient)
	return e, d, n

def modular_power(base, exp, modulus):
	"""
	Efficiently compute `(base ** exp) % modulus`, using exponentiation by
	squaring.
	"""

	result = 1
	base %= modulus

	while exp:
		if exp % 2 == 1:
			result = (result * base) % modulus
		exp >>= 1
		base = (base ** 2) % modulus

	return result

def encrypt(e, n, m):
	"""
	Encrypt `m` using public key `e` and modulus `n`.
	"""

	return modular_power(m, e, n)

def decrypt(d, n, c):
	"""
	Decrypt `c` using private key `d` and modulus `n`.
	"""

	return modular_power(c, d, n)

def is_prime(n, k=5):
	"""
	Test `n` for primality using the probabilistic Rabin-Miller test. Returns
	`true` if it's probably prime, and `false` if it's definitely composite.
	"""

	if n == 2:
		return True

	if n <= 1 or n % 2 == 0:
		return False

	s, d = decompose_to_factors_of_2(n - 1)

	def is_witness(a):
		"""
		Check whether `a` is a witness to the primality of `n`.
		"""

		x = modular_power(a, d, n)
		if x in [1, n - 1]:
			return False

		for _ in range(s - 1):
			x = modular_power(x, 2, n)
			if x == 1:
				return True

			if x == n - 1:
				return False

		return True

	for _ in range(k):
		if is_witness(random.randint(2, n - 1)):
			return False

	return True

def get_random_prime(num_bits):
	"""
	Create a random prime number of bit-length `num_bits`.
	"""

	lower_bound = 2 ** (num_bits - 2)
	upper_bound = 2 ** (num_bits - 1) - 1
	guess = random.randint(lower_bound, upper_bound)

	if guess % 2 == 0:
		guess += 1

	while not is_prime(guess):
		guess += 2

	return guess

def decompose_to_factors_of_2(num):
	"""
	Decompose `num` into the form `(2 ** s) * d`, and return `(s, d)`.
	"""

	s = 0
	d = num

	while d % 2 == 0:
		d //= 2
		s += 1

	return s, d

def unit_tests():
	"""
	Dead-simple unit tests for the functions defined inside this module.
	"""

	print("Running unit test assertions. Only failures will be reported.")
	assert gcd(5, 15) == 5
	assert gcd(15, 7) == 1
	assert coprime(8, 3)
	assert not coprime(8, 4)
	assert bezout_coefficients(180, 150) == (1, -1, 30)
	assert bezout_coefficients(17, 150) == (53, -6, 1)
	assert modular_inverse(17, 3120) == -367
	public, private, mod = create_key_pair(10)
	assert public is not None and private is not None and mod is not None
	assert encrypt(17, 3233, 65) == 2790
	assert decrypt(2753, 3233, 2790) == 65
	assert modular_power(9, 31, 20) == pow(9, 31, 20)
	assert decompose_to_factors_of_2(20) == (2, 5)
	assert decompose_to_factors_of_2(16) == (4, 1)
	assert decompose_to_factors_of_2(72) == (3, 9)

	first_100_primes = [
		2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
		71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139,
		149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
		227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293,
		307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383,
		389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463,
		467, 479, 487, 491, 499, 503, 509, 521, 523, 541]

	for num in range(first_100_primes[-1] + 1):
		assert is_prime(num) == (num in first_100_primes)

if __name__ == "__main__":
	unit_tests()
