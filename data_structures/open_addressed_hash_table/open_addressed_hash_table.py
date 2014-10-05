class OpenAddressedHashTable(object):
	"""
	A dynamically resized open-addressed hash-table.

	Attributes:
		_buckets (list): All the buckets (slots) that items can be inserted
			into in this table.
		_hash_func (lambda key, num_probes: index): A function that intakes a
			`key` (an item being hashed into the table) and `num_probes` (the
			number of attempted probes), and outputs `index`, in the range
			[0, num_buckets - 1].
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
		Args:
			item (item): An item to insert into this table. If it's already
				present, it will get discarded.
		"""

		ind = self.index(item)
		if ind == -1:
			return

		if self.load_factor() >= 0.5:
			self._resize()

		self._buckets[ind] = item
		self._length += 1

	def remove(self, item):
		pass

	def index(self, item):
		"""
		Args:
			item (item): The item to search for by probing the table.

		Returns:
			int: -1 if `item` was not found inside this table; otherwise, its
			index inside `_buckets`.
		"""

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
