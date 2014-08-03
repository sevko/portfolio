"""
An implementation of radix sort.
"""

import random
import math

def radix_sort(array, radix):
	"""
	Perform a radix_sort.

	Args:
		array (list of int): An array of integers.

	Returns:
		A pointer to the sorted copy of `array`.
	"""

	num_digits = max(
		math.log(abs(max(array)), radix),
		math.log(abs(min(array)), radix)
	)
	num_digits = int(num_digits) + 1

	for digit in xrange(num_digits):
		counts = [[] for digits_place in xrange(radix * 2 - 1)]

		for num in array:
			curr_digit = int(
				abs(num) % (radix ** (digit + 1)) / (radix ** digit)
			)
			curr_digit *= (-1 if num < 0 else 1)
			counts[curr_digit + radix - 1].append(num)

		array = [num for group in counts for num in group]

	return array

def test_radix_sort():
	"""
	Test `radix_sort()` against Python's inbuilt `sorted()`.

	If `radix_sort()` fails to correctly sort an array, print a message
	containing both the original array and the array after the attempted sort.
	"""

	for i in xrange(100):
		arr = range(-50, 50)
		random.shuffle(arr)

		sorted_arr = radix_sort(arr, 10)
		if sorted_arr != sorted(arr):
			print (
				"radix sort() failed.\n"
				"\tOriginal: %s\n\tAttempted sort: %s"
			) % (arr, sorted_arr)

if __name__ == "__main__":
	test_radix_sort()
