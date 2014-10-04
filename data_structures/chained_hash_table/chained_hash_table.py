class ChainedHashTable(object):

	def __init__(self, num_buckets, hash_func):
		self._buckets = [[] for _ in xrange(num_buckets)]
		self._hash_func = hash_func
		self._length = 0

	def insert(self, item):
		self._get_bucket(item).append(item)

	def remove(self, item):
		self._get_bucket(item).remove(item)

	def contains(self, item):
		return item in self._get_bucket(item)

	def _get_bucket(self, item):
		return self._buckets[self._hash_func(item)]

	def load_balance():
		pass

	def __str__(self):
		return str(self._buckets)
