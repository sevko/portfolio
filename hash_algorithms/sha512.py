"""
`sha512` contains an implementation of the SHA-512 hash algorithm.

It adheres as much as possible to the pseudocode in
`http://en.wikipedia.org/wiki/SHA-2` for purposes of readability. Some of the
code is thus a little inelegant, unoptimized, and PEP8-incompliant.
"""

import hashlib

def sha512(string):
	"""
	Generate the SHA-512 digest of a string.

	Args:
		string (str): The string to process.

	Returns:
		(int) The hash of `str`.
	"""

	def rightRotate(x, c):
		"""
		Perform a circular 64-bit right-rotation.

		Args:
			x (int): The value to rotate.
			c (int): The number of bits to rotate `x` by.

		Returns:
			(int) `x` shifted right `c` bits, with those that overflowed
			reappearing on the left.
		"""
		return (x >> c) | (x << (64 - c))

	# Initialize hash constants.
	h0 = 0x6A09E667F3BCC908
	h1 = 0xBB67AE8584CAA73B
	h2 = 0x3C6EF372FE94F82B
	h3 = 0xA54FF53A5F1D36F1
	h4 = 0x510E527FADE682D1
	h5 = 0x9B05688C2B3E6C1F
	h6 = 0x1F83D9ABFB41BD6B
	h7 = 0x5BE0CD19137E2179

	# Initialize array of round constants.
	k = [
		0x428A2F98D728AE22, 0x7137449123EF65CD, 0xB5C0FBCFEC4D3B2F,
		0xE9B5DBA58189DBBC, 0x3956C25BF348B538, 0x59F111F1B605D019,
		0x923F82A4AF194F9B, 0xAB1C5ED5DA6D8118, 0xD807AA98A3030242,
		0x12835B0145706FBE, 0x243185BE4EE4B28C, 0x550C7DC3D5FFB4E2,
		0x72BE5D74F27B896F, 0x80DEB1FE3B1696B1, 0x9BDC06A725C71235,
		0xC19BF174CF692694, 0xE49B69C19EF14AD2, 0xEFBE4786384F25E3,
		0x0FC19DC68B8CD5B5, 0x240CA1CC77AC9C65, 0x2DE92C6F592B0275,
		0x4A7484AA6EA6E483, 0x5CB0A9DCBD41FBD4, 0x76F988DA831153B5,
		0x983E5152EE66DFAB, 0xA831C66D2DB43210, 0xB00327C898FB213F,
		0xBF597FC7BEEF0EE4, 0xC6E00BF33DA88FC2, 0xD5A79147930AA725,
		0x06CA6351E003826F, 0x142929670A0E6E70, 0x27B70A8546D22FFC,
		0x2E1B21385C26C926, 0x4D2C6DFC5AC42AED, 0x53380D139D95B3DF,
		0x650A73548BAF63DE, 0x766A0ABB3C77B2A8, 0x81C2C92E47EDAEE6,
		0x92722C851482353B, 0xA2BFE8A14CF10364, 0xA81A664BBC423001,
		0xC24B8B70D0F89791, 0xC76C51A30654BE30, 0xD192E819D6EF5218,
		0xD69906245565A910, 0xF40E35855771202A, 0x106AA07032BBD1B8,
		0x19A4C116B8D2D0C8, 0x1E376C085141AB53, 0x2748774CDF8EEB99,
		0x34B0BCB5E19B48A8, 0x391C0CB3C5C95A63, 0x4ED8AA4AE3418ACB,
		0x5B9CCA4F7763E373, 0x682E6FF3D6B2B8A3, 0x748F82EE5DEFB2FC,
		0x78A5636F43172F60, 0x84C87814A1F0AB72, 0x8CC702081A6439EC,
		0x90BEFFFA23631E28, 0xA4506CEBDE82BDE9, 0xBEF9A3F7B2C67915,
		0xC67178F2E372532B, 0xCA273ECEEA26619C, 0xD186B8C721C0C207,
		0xEADA7DD6CDE0EB1E, 0xF57D4F7FEE6ED178, 0x06F067AA72176FBA,
		0x0A637DC5A2C898A6, 0x113F9804BEF90DAE, 0x1B710B35131C471B,
		0x28DB77F523047D84, 0x32CAAB7B40C72493, 0x3C9EBE0A15C9BEBC,
		0x431D67C49C100D4C, 0x4CC5D4BECB3E42B6, 0x597F299CFC657E2A,
		0x5FCB6FAB3AD6FAEC, 0x6C44198C4A475817
	]

	bin_msg = "".join([bin(ord(byte))[2:].zfill(8) for byte in string])
	bin_msg += "1" + (896 - 1 - len(bin_msg) % 1024) * "0"
	bin_msg += bin(len(string) * 8)[2:].zfill(128)

	# Partition bin_msg into 1024-bit chunks.
	for chunk in range(len(bin_msg) / 1024):

		# Partition chunk into an array 'w' of 16 64-bit words.
		w = []
		for word in range(16):
			start = chunk * 1024 + word * 64
			w.append(bin_msg[start:(start + 64)])

		# Extend original 16-word array to 80 words.
		for i in range(16, 80):
			s0 = rightRotate(int(w[i - 15], 2), 1) ^ \
				rightRotate(int(w[i - 15], 2), 8) ^ (int(w[i - 15], 2) >> 7)
			s1 = rightRotate(int(w[i - 2], 2), 19) ^ \
				rightRotate(int(w[i - 2], 2), 61) ^ (int(w[i - 2], 2) >> 6)
			total = (int(w[i - 16], 2) + s0 + int(w[i - 7], 2) + s1) % 2 ** 64
			w.append(bin(total)[2:])

		# Initialize word buffers.
		a = h0
		b = h1
		c = h2
		d = h3
		e = h4
		f = h5
		g = h6
		h = h7

		# Primary for-loop; calculates hash.
		for i in range(80):
			s1 = rightRotate(e, 14) ^ rightRotate(e, 18) ^ rightRotate(e, 41)
			ch = (e & f) ^ (~e & g)
			temp1 = h + s1 + ch + k[i] + int(w[i], 2)
			s0 = rightRotate(a, 28) ^ rightRotate(a, 34) ^ rightRotate(a, 39)
			maj = (a & b) ^ (a & c) ^ (b & c)
			temp2 = s0 + maj

			# Update word buffers.
			h = g
			g = f
			f = e
			e = (d + temp1) % 2 ** 64
			d = c
			c = b
			b = a
			a = (temp1 + temp2) % 2 ** 64

		# Update hash constants
		h0 = (h0 + a) % 2 ** 64
		h1 = (h1 + b) % 2 ** 64
		h2 = (h2 + c) % 2 ** 64
		h3 = (h3 + d) % 2 ** 64
		h4 = (h4 + e) % 2 ** 64
		h5 = (h5 + f) % 2 ** 64
		h6 = (h6 + g) % 2 ** 64
		h7 = (h7 + h) % 2 ** 64

	constants = [h0, h1, h2, h3, h4, h5, h6, h7]
	digest = 0
	for ind in xrange(len(constants)):
		digest += constants[ind] << (64 * (len(constants) - ind - 1))
	return digest

if __name__ == "__main__":
	msg = "The quick brown fox jumps over the lazy dog"
	expected = int(hashlib.sha512(msg).hexdigest(), 16)
	hash_digest = sha512(msg)
	print "Hash of '%s': %x\nMatches expected: %s." % (
		msg, hash_digest, hash_digest == expected
	)
