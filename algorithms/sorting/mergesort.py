"""
An implementation of mergesort.
"""

import random

def mergesort(array):
	"""
	Perform a mergesort.

	Args:
		array (list): An array without elements equal in value.

	Returns:
		A pointer to the sorted copy of `array`.
	"""

	if len(array) == 1:
		return array

	length = len(array)
	left = mergesort(array[:(length / 2)])
	right = mergesort(array[(length / 2):])

	sorted_arr = []
	left_ind = right_ind = 0

	while left_ind < len(left) and right_ind < len(right):
		if left[left_ind] < right[right_ind]:
			sorted_arr.append(left[left_ind])
			left_ind += 1
		else:
			sorted_arr.append(right[right_ind])
			right_ind += 1
	else:
		sorted_arr += right[right_ind:]
		sorted_arr += left[left_ind:]

	return sorted_arr

def test_mergesort():
	"""
	Test `mergesort()` against Python's inbuilt `sorted()`.

	If `mergesort()` fails to correctly sort an array, print a message
	containing both the original array and the array after the attempted sort.
	"""

	for i in xrange(100):
		arr = range(50)
		random.shuffle(arr)

		mergesorted_arr = mergesort(arr)
		if mergesorted_arr != sorted(arr):
			print (
				"mergesort() failed.\n"
				"\tOriginal: %s\n\tAttempted sort: %s"
			) % (arr, mergesorted_arr)

if __name__ == "__main__":
	test_mergesort()
