class Heap(object):

	def __init__(self):
		self._items = []

	def insert(self, item):
		self._items.append(item)

		def get_parent_ind(ind):
			return int((ind - 1) / 2)

		child_ind = len(self) - 1
		parent_ind = get_parent_ind(child_ind)
		while self[parent_ind] < self[child_ind] and child_ind >= 0:
			temp = self[parent_ind]
			self[parent_ind] = self[child_ind]
			self[child_ind] = temp
			child_ind = parent_ind

	def remove(self):
		pass

	def __getitem__(self, ind):
		return self._items[ind]

	def __length__(self):
		return len(self._items)
