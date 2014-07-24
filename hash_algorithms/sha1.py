"""
`sha1` contains an implementation of the SHA-1 hash algorithm.

It adheres to the pseudocode in `http://en.wikipedia.org/wiki/SHA-1` (checked
against official documentation). Some of the code is thus a little inelegant,
unoptimized, and PEP8-incompliant.
"""

def sha1(string):
	"""
	Return the SHA1 digest of a string.

	Args:
		string (str): The string to process.

	Returns:
		(int) The SHA1 hash, in numeric form, of `string`.
	"""

	def leftRotate(x, c):
		"""
		Perform a circular left-rotation.

		Args:
			x (int): A 32-bit int.
			c (int): The number of bits to shift `x` by.

		Returns:
			`x` rotated circularly by `c` bits.
		"""

		return (x << c) | (x >> (32 - c))

	# Initialize SHA1 hash constants.
	h0 = 0x67452301
	h1 = 0xEFCDAB89
	h2 = 0x98BADCFE
	h3 = 0x10325476
	h4 = 0xC3D2E1F0

	# Convert `string` to its binary representation, and pad each byte with
	# 0 for a total length of 8.
	bin_msg = "".join([bin(ord(byte))[2:].zfill(8) for byte in string])
	bin_msg += "1" + (448 - 1 - len(bin_msg) % 512) * "0"
	bin_msg += bin(len(string) * 8)[2:].zfill(64)

	# Partition `bin_msg` into 512-bit chunks.
	for chunk in range(len(bin_msg) / 512):

		# Partition each 512-bit chunk into `w`, an array of 16 32-bit words.
		w = []
		for word in range(16):
			start = chunk * 512 + word * 32
			w.append(bin_msg[start:(start + 32)])

		# Extend the original 16-word array `w` to 80 words.
		for i in range(16, 80):
			val = (
				int(w[i - 3], 2) ^ int(w[i - 8], 2) ^ int(w[i - 14], 2) ^
				int(w[i - 16], 2)
			)
			w.append(bin(leftRotate(val, 1) % 2 ** 32))

		# Initialize word buffers.
		a = h0
		b = h1
		c = h2
		d = h3
		e = h4

		# Primary for-loop that hashes message.
		for i in range(80):

			# Round 1
			if 0 <= i and i <= 19:
				f = ((b & c) | (~b & d))
				k = 0x5A827999

			# Round 2
			elif 20 <= i and i <= 39:
				f = (b ^ c ^ d)
				k = 0x6ED9EBA1

			# Round 3
			elif 40 <= i and i <= 59:
				f = ((b & c) | (b & d) | (c & d))
				k = 0x8F1BBCDC

			# Round 4
			elif 60 <= i and i <= 79:
				f = (b ^ c ^ d)
				k = 0xCA62C1D6

			# Update the word buffers.
			temp = (leftRotate(a, 5) + f + e + k + int(w[i], 2)) % 2**32
			e = d
			d = c
			c = leftRotate(b, 30) % 2**32
			b = a
			a = temp

		# Update the hash constants.
		h0 = (h0 + a) % 2 ** 32
		h1 = (h1 + b) % 2 ** 32
		h2 = (h2 + c) % 2 ** 32
		h3 = (h3 + d) % 2 ** 32
		h4 = (h4 + e) % 2 ** 32

	return (h0 << 128) | (h1 << 96) | (h2 << 64) | (h3 << 32) | h4

if __name__ == "__main__":
	msg = "The quick brown fox jumps over the lazy dog"
	expected = 0x2fd4e1c67a2d28fced849ee1bb76e7391b93eb12
	hex_hash = sha1(msg)
	print "Hash of '%s': %s\nMatches expected: %s." % (
		msg, hex_hash, hex_hash == expected
	)
