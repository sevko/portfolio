"""
Unit tests for the open_addressed_hash_table module.
"""

import unittest
import open_addressed_hash_table

class TestOpenAddressedHashTable(unittest.TestCase):
	"""
	Tests for `open_addressed_hash_table.OpenAddressedHashTable`

	Attributes:
		table (OpenAddressedHashTable): A table to be used for testing.
	"""

	def setUp(self):
		"""
		Initialize `self.table`.
		"""

		def hash_func(item, num_probes):
			return item + num_probes

		self.table = open_addressed_hash_table.OpenAddressedHashTable(
			10, hash_func
		)

	def test_insert(self):
		"""
		Test `OpenAddressedHashTable.insert()`.
		"""

		self.table.insert(1)
		self.assertEqual(len(self.table), 1)
		self.table.insert(2)
		self.assertEqual(len(self.table), 2)
		self.table.insert(1)
		self.assertEqual(len(self.table), 2)

	def test_index(self):
		"""
		Test `OpenAddressedHashTable.index()`.
		"""

		# finds existing items
		self.table._buckets[2] = 2
		self.assertEqual(self.table.index(2), 2)
		self.table._buckets[3] = 12
		self.assertEqual(self.table.index(12), 3)

		# finds None index
		self.assertEqual(self.table.index(3), 4)
		self.table._buckets[2] = self.table._vacant
		self.assertEqual(self.table.index(12), 3)

		# finds vacant index
		self.table._buckets[1] = 1
		self.assertEqual(self.table.index(11), 2)

	def test_resize(self):
		"""
		Test `OpenAddressedHashTable._resize()`.
		"""

		self.assertEqual(len(self.table._buckets), 10)
		for num in xrange(4):
			self.table.insert(num)
		self.assertEqual(len(self.table._buckets), 10)
		self.table.insert(5)
		self.assertEqual(len(self.table._buckets), 20)

if __name__ == "__main__":
	unittest.main(verbosity=2)
