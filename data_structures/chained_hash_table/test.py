import unittest
import chained_hash_table

class Test(unittest.TestCase):

	num_buckets = 5

	def setUp(self):
		def hash_func(num):
			return num % self.num_buckets

		self.table = chained_hash_table.ChainedHashTable(
			self.num_buckets, hash_func
		)

	def test_insert(self):
		for value in xrange(self.num_buckets):
			self.table.insert(value)
			self.assertEqual(len(self.table._buckets[value]), 1)
			self.table.insert(value + self.num_buckets)
			self.assertEqual(len(self.table._buckets[value]), 2)

	def test_remove(self):
		self.table.insert(5)
		self.table.insert(6)
		self.table.remove(8)
		self.assertEqual(len(self.table), 2)
		self.table.remove(5)
		self.assertEqual(len(self.table._buckets[0]), 0)

if __name__ == "__main__":
	unittest.main()
