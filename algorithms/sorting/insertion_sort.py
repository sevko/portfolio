"""
An implementation on an in-place insertion sort.
"""

import random

def insertion_sort(array):
	"""
	Perform an in-place insertion sort.

	Args:
		array (list): The array to sort. Will be modified!

	Returns:
		A pointer to the argument array, now sorted.
	"""

	for sortInd in xrange(1, len(array)):
		for posInd in xrange(sortInd, 0, -1):
			if array[posInd] < array[posInd - 1]:
				tmp = array[posInd]
				array[posInd] = array[posInd - 1]
				array[posInd - 1] = tmp
			else:
				break

	return array

def test_insertion_sort():
	"""
	Test `insertion_sort()` against Python's inbuilt `sorted()`.
	"""

	for i in xrange(100):
		arr = [random.randint(0, 20) for num in xrange(30)]
		if insertion_sort(arr) != sorted(arr):
			print insertion_sort(arr)

if __name__ == "__main__":
	test_insertion_sort()
