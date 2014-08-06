"""
An implementation of a binary search.
"""

import random

def binary_search(array, value):
	"""
	Perform a binary search for a value in an array.

	Args:
		array (list of int): A sorted array.
		value (int): The value to search for.

	Returns:
		If `value` is inside `array`, return its index; otherwise, -1.
	"""

	left = 0
	right = len(array) - 1

	while left <= right:
		middle = (left + right) / 2
		if array[middle] == value:
			return middle
		elif array[middle] < value:
			left = middle + 1
		else:
			right = middle - 1

	return -1

def test_binary_search():
	"""
	Test `binary_search()` against Python inbuilt `index()`.

	Test the result of `binary_search()`, performed on an array of ordered
	elements with a randomly generated serach value, against that of Python
	lists' `index()` in 100 trials.
	"""

	for round in xrange(100):
		upper_bound = 200
		search_space = range(upper_bound)
		value = random.randint(0, int(upper_bound * 1.2))
		try:
			real_pos = search_space.index(value)
		except ValueError:
			real_pos = -1
		assert binary_search(search_space, value) == real_pos

if __name__ == "__main__":
	test_binary_search()
