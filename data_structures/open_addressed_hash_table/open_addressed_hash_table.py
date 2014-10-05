class OpenAddressedHashTable(object):
	"""
	A dynamically resized open-addressed hash-table.

	Attributes:
		_buckets (list): All the buckets (slots) that items can be inserted
			into in this table.
		_hash_func (lambda key, num_probes: index): A function that intakes a
			`key` (an item being hashed into the table) and `num_probes` (the
			number of attempted probes), and outputs a numeric hash value.
		_length (int): The number of items contained in this table.
		_vacant (list): A sentinel value pointed to by positions in the table
			after they contained an item that was removed. Used by
			`index()` to keep searching, instead of stopping at what it
			would otherwise perceive as `None`.
	"""

	def __init__(self, num_buckets, hash_func, items=None):
		"""
		Args:
			num_buckets (int): The number of buckets (slots that items
				can be inserted into) to initialize this table with
			hash_func (function): see `OpenAddressedHashTable` docstring.
			items (list, optional): Items to optionally immediately insert into
				the table; defaults to None.
		"""

		self._buckets = [None for _ in xrange(num_buckets)]
		self._hash_func = hash_func
		self._length = 0
		self._vacant = []

		if items is not None:
			for item in items:
				self.insert(item)

	def insert(self, item):
		"""
		Inserts an item into this table if it's not already present; resizes if
		the insertion increased the load-factor to (more than) 0.5.

		Args:
			item (object): An item to insert into this table. If it's already
				present, it will get discarded.
		"""

		ind = self.index(item)
		if self._buckets[ind] == item:
			return

		self._buckets[ind] = item
		self._length += 1

		if self.load_factor() >= 0.5:
			self._resize()

	def remove(self, item):
		"""
		Args:
			item (object): An item to remove from this hash table; its position
				will be set equal to `self._vacant`. If no such item is found,
				nothing happens.
		"""

		ind = self.index(item)
		if self._buckets[ind] == item:
			self._buckets[ind] = self._vacant
			self._length -= 1

	def index(self, item):
		"""
		Args:
			item (item): The item to search for by probing the table.

		Returns:
			int: If `item` is not found, the index of either the first
			encountered occurence of "vacant" (see `OpenAddressedHashTable`
			docstring) or None. Otherwise, the index of `item`. This seems
			convoluted, but it's flexible enough to be used to check
			whether an item exists or not, and, depending on that, to
			insert/remove a value.
		"""

		num_probes = 0
		curr_ind = self._hash_func(item, num_probes) % len(self._buckets)
		vacant_ind = -1

		while True:
			if self._buckets[curr_ind] is None:
				return vacant_ind if vacant_ind != -1 else curr_ind
			elif self._buckets[curr_ind] is self._vacant and vacant_ind == -1:
				vacant_ind = curr_ind
			elif self._buckets[curr_ind] == item:
				return curr_ind
			num_probes += 1
			curr_ind = self._hash_func(item, num_probes) % len(self._buckets)

	def load_factor(self):
		"""
		Returns:
			The load-factor of this hash table.
		"""

		return float(self._length) / len(self._buckets)

	def _resize(self):
		"""
		Double the size of this hash table.
		"""

		buckets = self._buckets
		self._buckets = [None for _ in xrange(len(buckets) * 2)]
		self._length = 0
		for item in buckets:
			if item is not None and item is not self._vacant:
				self.insert(item)

	def __len__(self):
		return self._length

	def __str__(self):
		strings = []
		for item in self._buckets:
			strings.append("vacant" if item is self._vacant else str(item))
		return "[%s]" % ", ".join(strings)
