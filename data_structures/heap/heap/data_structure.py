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
		def get_child_inds(ind):
			next_level = ind * 2
			return next_level + 1, next_level + 2

		if len(self) == 1:
			root = self._items.pop()
		else:
			root = self[0]
			self[0] = self._items.pop()

			curr_ind = 0
			while True:
				child_inds = get_child_inds(curr_ind)
				left_greater = child_inds[0] < len(self) and \
					self[curr_ind] < self[child_inds[0]]
				right_greater = child_inds[1] < len(self) and \
					self[curr_ind] < self[child_inds[1]]

				if right_greater and right_greater:
					if self[child_inds[1]] > self[child_inds[0]]:
						child_ind = child_inds[1]
					else:
						child_ind = child_inds[0]
				elif left_greater:
					child_ind = child_inds[0]
				elif right_greater:
					child_ind = child_inds[1]
				else:
					break

				temp = self[child_ind]
				self[child_ind] = self[curr_ind]
				self[curr_ind] = temp
				curr_ind = child_ind

		return root

	def __getitem__(self, key):
		return self._items[key]

	def __setitem__(self, key, value):
		self._items[key] = value

	def __len__(self):
		return len(self._items)

	def __str__(self):
		"""
		Returns:
			(str) `self` in a multi-line, pseudo-tree string format.
		"""

		rows = []
		curr_ind = 0
		while curr_ind < len(self):
			next_ind = curr_ind * 2 + 1
			rows.append(str(item) for item in self[curr_ind:next_ind])
			curr_ind = next_ind
		return "\n".join([", ".join(row) for row in rows])
