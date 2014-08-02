import random

def quicksort(array):
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
		return quicksort(lesser) + quicksort(greater)

def test_quicksort():
	"""
	Test `quicksort()` against Python's inbuilt `sorted()`.
	"""

	for i in xrange(100):
		arr = range(50)
		random.shuffle(arr)
		if quicksort(arr) != sorted(arr):
			print arr

if __name__ == "__main__":
	test_quicksort()
