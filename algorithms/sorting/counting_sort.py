"""
An implementation of counting sort.
"""

import random

def counting_sort(array):
	"""
	Perform a counting sort.

	Args:
		array (list of ints): An array of integers.

	Returns:
		A pointer to the sorted copy of `array`.
	"""

	min_val = min(array)
	max_val = max(array)
	counts = [0] * (max_val - min_val + 1)

	for item in array:
		counts[item + min_val] += 1

	sorted_arr = []
	for ind in xrange(len(counts)):
		for repetition in xrange(counts[ind]):
			sorted_arr.append(ind + min_val)

	return sorted_arr

def test_counting_sort():
	"""
	Test `counting_sort()` against Python's inbuilt `sorted()`.

	If `counting_sort()` fails to correctly sort an array, print a message
	containing both the original array and the array after the attempted sort.
	"""

	for i in xrange(100):
		arr = range(-50, 50)
		random.shuffle(arr)

		sorted_arr = counting_sort(arr)
		if sorted_arr != sorted(arr):
			print (
				"counting sort() failed.\n"
				"\tOriginal: %s\n\tAttempted sort: %s"
			) % (arr, sorted_arr)

if __name__ == "__main__":
	test_counting_sort()
