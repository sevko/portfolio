"""
Unit tests for the `heap` package.
"""

import unittest
import heap

class TestHeap(unittest.TestCase):
	"""Unit tests for `heap.Heap`.
	"""

	def setUp(self):
		"""Create an instance of Heap() to use for testing.
		"""

		self.test_heap = heap.Heap()

	def test_insert(self):
		"""Test `Heap.insert()`.

		Check the composition of the heap's internal tree after the
		insertion of a number of items.
		"""

		states = [
			(1, [1]),
			(2, [2, 1]),
			(3, [3, 1, 2]),
			(4, [4, 3, 2, 1]),
			(5, [5, 4, 2, 1, 3]),
		]

		for state in states:
			self.test_heap.insert(state[0])
			self.assertEqual(
				self.test_heap._items, state[1],
				"State %s, after insertion of '%d', matches expected: %s." % (
					self.test_heap._items, state[0], state[1]
				)
			)

if __name__ == "__main__":
	unittest.main(verbosity=2)
