"""
A naive implementation of a chained hash table.
"""

class ChainedHashTable(object):
	"""
	A chained hash table.

	Attributes:
		_buckets (list of list): h
		_hash_func (lambda x: y): The hash function used to determine the
			index of the bucket that any inserted/removed/searched item
			resides in. Output value must be: `0 <= out < num_buckets`.
		_length (int): The total number of items contained in this table.
	"""

	def __init__(self, num_buckets, hash_func):
		"""
		Args:
			num_buckets (int): The number of buckets this table will use.
			hash_func: see `ChainedHashTable` docstring.
		"""

		self._buckets = [[] for _ in xrange(num_buckets)]
		self._hash_func = hash_func
		self._length = 0

	def insert(self, item):
		"""
		Args:
			item (object): An item to insert into this table.
		"""

		self._length += 1
		self._get_bucket(item).append(item)

	def remove(self, item):
		"""
		Args:
			item (object): An item to remove from this table.
		"""

		self._length -= 1
		self._get_bucket(item).remove(item)

	def contains(self, item):
		"""
		Args:
			item (object): The item to search this table for.

		Returns:
			bool: True if `item` is found; false otherwise.
		"""

		return item in self._get_bucket(item)

	def load_balance(self):
		"""
		Returns:
			(float) The load-balance (quotient of number of elements and
			number of buckets) of this table.
		"""

		return float(len(self)) / len(self._buckets)

	def _get_bucket(self, item):
		"""
		Args:
			item (object): The item to find an appropriate bucket for.

		Returns:
			list: The bucket to which `item` belongs, based on the output of
			`self.hash_func()`.
		"""

		return self._buckets[self._hash_func(item)]

	def __len__(self):
		return self._length

	def __str__(self):
		return str(self._buckets)
