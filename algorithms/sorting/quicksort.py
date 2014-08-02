"""
Implementations of both an out-of-place and in-place quicksort.
"""

import random

def quicksort_out_of_place(array):
	"""
	Perform an out-of-place quicksort.

	Args:
		array (list): An array without elements equal in value.

	Returns:
		A pointer to the sorted copy of `array`.
	"""

	if len(array) <= 1:
		return array
	else:
		lesser = []
		greater = []
		pivot = random.choice(array)
		for ind in xrange(len(array)):
			if array[ind] < pivot:
				lesser.append(array[ind])
			else:
				greater.append(array[ind])
		return quicksort_out_of_place(lesser) + quicksort_out_of_place(greater)

def quicksort_in_place(array):
	"""
	Perform an in-place quicksort.

	Simply a wrapper for `quicksort_in_place_aux`, which contains the actual
	quicksort implementation.

	Args:
		array (list): An array without elements equal in value. Will be
			modified.

	Returns:
		A pointer to the sorted copy of `array`.
	"""

	def quicksort_in_place_aux(array, start, end):
		"""
		A helper for `quicksort_in_place()`.

		Args:
			array (list): The array to sort.
			start (int): The starting index of the current partition, within
				which numbers will be grouped around a pivot.
			end (int): The ending index of the current partition, within
				which numbers will be grouped around a pivot.
		"""

		if end <= start:
			return

		pivot_ind = random.randint(start, end)
		pivot = array[pivot_ind]
		left = start
		right = end

		while left < right:
			while array[left] < pivot and left < right:
				left += 1

			while pivot < array[right] and left < right:
				right -= 1

			if left != right:
				tmp = array[left]
				array[left] = array[right]
				array[right] = tmp

		quicksort_in_place_aux(array, start, left)
		quicksort_in_place_aux(array, left + 1, end)

	quicksort_in_place_aux(array, 0, len(array) - 1)
	return array

def test_quicksort():
	"""
	Test `quicksort_out_of_place()` and `quicksort_in_place()` against Python's
	inbuilt `sorted()`.

	If either function fails to correctly sort an array, print a message
	containing both the original array and the array after the attempted sort.
	"""

	for i in xrange(100):
		arr = range(50)
		random.shuffle(arr)
		original = list(arr)

		sorted_out_of_place = quicksort_out_of_place(arr)
		if sorted_out_of_place != sorted(original):
			print (
				"quicksort_out_of_place() failed.\n"
				"\tOriginal: %s\n\tAttempted sort: %s"
			) % (original, sorted_out_of_place)

		if quicksort_in_place(arr) != sorted(original):
			print (
				"quicksort_in_place() failed.\n"
				"\tOriginal: %s\n\tAttempted sort: %s"
			) % (original, arr)

if __name__ == "__main__":
	test_quicksort()
