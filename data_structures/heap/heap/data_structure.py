class Heap(object):

	def __init__(self):
		self._items = []

	def insert(self, *items):
		def get_parent_ind(ind):
			return int((ind - 1) / 2)

		for item in items:
			self._items.append(item)

			child_ind = len(self) - 1
			parent_ind = get_parent_ind(child_ind)
			while child_ind > 0 and self[parent_ind] < self[child_ind]:
				temp = self[parent_ind]
				self[parent_ind] = self[child_ind]
				self[child_ind] = temp
				child_ind = parent_ind
				parent_ind = get_parent_ind(child_ind)

	def remove(self):
		pass

	def __getitem__(self, key):
		return self._items[key]

	def __setitem__(self, key, value):
		self._items[key] = value

	def __len__(self):
		return len(self._items)
