class OpenAddressedHashTable(object):
	def __init__(self, num_buckets, hash_func, items=None):
		self._buckets = [None for _ in xrange(num_buckets)]
		self._hash_func = hash_func
		self._length = 0
		self._vacant = []

		if items is not None:
			for item in items:
				self.insert(item)

	def insert(self, item):
		ind = self.contains(item)
		if ind == -1:
			return

		if self.load_factor() >= 0.5:
			self._resize()

		self._buckets[ind] = item
		self._length += 1

	def remove(self, item):
		pass

	def contains(self, item):
		num_probes = 0
		curr_ind = self._hash_func(item, num_probes)
		while self._buckets[curr_ind] is not None and \
			self._buckets[curr_ind] is not self._vacant:
			if self._buckets[curr_ind] == item:
				return -1
			num_probes += 1
			curr_ind = self._hash_func(item, num_probes)

		return curr_ind


	def load_factor(self):
		pass

	def _resize(self):
		pass

	def __len__(self):
		return self._length

	def __str__(self):
		return str(self._buckets)
