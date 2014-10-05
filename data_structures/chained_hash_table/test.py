"""
`unittest` unit tests for the chained_hash_table module.
"""

import unittest
import chained_hash_table

class TestChainedHashTable(unittest.TestCase):
	"""
	Unit-tests for `chained_hash_table.ChainedHashTable`.
	"""

	num_buckets = 5 # Number of buckets in the test ChainedHashTable.

	def setUp(self):
		"""Create an instance of ChainedHashTable() to use for testing.
		"""

		def hash_func(num):
			return num % self.num_buckets

		self.table = chained_hash_table.ChainedHashTable(
			self.num_buckets, hash_func
		)

	def test_insert(self):
		"""Test ChainedHashTable.insert().
		"""

		for value in xrange(self.num_buckets):
			self.table.insert(value)
			self.assertEqual(len(self.table._buckets[value]), 1)
			self.table.insert(value + self.num_buckets)
			self.assertEqual(len(self.table._buckets[value]), 2)

	def test_remove(self):
		"""Test ChainedHashTable.remove().
		"""

		self.table.insert(5)
		self.table.insert(6)
		self.table.remove(8)
		self.assertEqual(len(self.table), 2)
		self.table.remove(5)
		self.assertEqual(len(self.table._buckets[0]), 0)

	def test_contains(self):
		"""Test ChainedHashTable.contains().
		"""

		self.table.insert(5)
		self.table.insert(6)
		self.assertTrue(self.table.contains(5))
		self.assertTrue(self.table.contains(6))
		self.assertFalse(self.table.contains(8))

	def test_load_factor(self):
		"""Test ChainedHashTable.load_factor().
		"""

		self.table.insert(5)
		self.table.insert(6)
		self.table.insert(8)
		self.assertEqual(self.table.load_factor(), 3.0 / self.num_buckets)

if __name__ == "__main__":
	unittest.main(verbosity=2)
