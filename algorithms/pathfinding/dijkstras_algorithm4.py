import heapq
import termcolor

class Grid:

	def __init__(self, grid_costs):
		self.height = len(grid_costs)
		self.width = len(grid_costs[0])
		self.grid_costs = grid_costs

	def dijkstras_algorithm(self, start, end):
		if not self._is_coord_traversable(start):
			raise ValueError("The start coord is invalid")

		elif not self._is_coord_traversable(end):
			raise ValueError("The end coord is invalid")

		costs_so_far = {start: 0}
		came_from = {start: None}
		frontier = [(0, start)]

		while frontier:
			curr_cost, curr_coords = heapq.heappop(frontier)
			if curr_coords == end:
				break

			for neighbor_coords in self._neighbor_coords(curr_coords):
				x, y = neighbor_coords
				neighbor_cost = curr_cost + self.grid_costs[y][x]

				if neighbor_coords not in costs_so_far or \
					neighbor_cost < costs_so_far[neighbor_coords]:
					costs_so_far[neighbor_coords] = neighbor_cost
					came_from[neighbor_coords] = curr_coords
					heapq.heappush(frontier, (neighbor_cost, neighbor_coords))

		else:
			return None

		path = []
		while curr_coords is not None:
			path.append(curr_coords)
			curr_coords = came_from[curr_coords]

		return path

	def _neighbor_coords(self, coord):
		x, y = coord
		all_neighbor_coords = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
		return list(filter(self._is_coord_traversable, all_neighbor_coords))

	def _is_coord_traversable(self, coord):
		x, y = coord
		return 0 <= x < self.width and \
			0 <= y < self.height and \
			self.grid_costs[y][x] is not None

	def to_ascii(self, path=None):
		grid_chars = [
			["█" if cost is None else str(cost) for cost in row]
			for row in self.grid_costs]

		if path is not None:
			start_x, start_y = path[0]
			grid_chars[start_y][start_x] = termcolor.colored(
				"A", "blue", attrs=["reverse"])

			end_x, end_y = path[-1]
			grid_chars[end_y][end_x] = termcolor.colored(
				"B", "blue", attrs=["reverse"])

			for (x, y) in path[1:-1]:
				grid_chars[y][x] = termcolor.colored("*", "blue")

		return "\n".join(" ".join(row) for row in grid_chars)

	@classmethod
	def from_ascii(cls, grid_str):
		return Grid([
			[None if char == "_" else int(char)
				for char in row.strip().split(" ")]
			for row in grid_str.strip().split("\n")])

if __name__ == "__main__":
	grid = Grid.from_ascii("""
		1 1 1 1 1 _ 1 1 1 _ 1 1
		1 1 1 1 1 9 9 _ _ _ 1 1
		1 _ _ _ 1 _ 9 _ 1 1 1 1
		1 _ 1 1 1 _ 9 1 1 1 1 1
		1 1 _ 1 1 _ 1 1 _ _ 1 1
		1 _ _ _ 9 _ 1 1 _ 1 1 1
		1 1 1 1 9 1 _ 1 _ 4 7 9
		1 1 _ 1 1 1 1 1 _ 5 7 9
		1 1 _ 1 _ _ _ _ _ 5 2 1
		1 1 _ 1 _ 1 1 1 _ 5 7 1
		1 1 _ 1 1 1 _ 1 _ 1 1 1
	""")
	# grid = Grid([
		# [1, 1, 1, 1, 1, 1, None, 1, 1, 1, 1],
		# [1, None, 1, 1, 1, 1, None, 1, None, 1, 1],
		# [1, 1, None, None, None, 1, None, 1, None, 1, 1],
		# [1, 1, None, 1, 1, 1, None, 1, None, 1, 1],
		# [1, 1, 1, 1, None, 1, 1, 1, None, 1, 1]
	# ])
	path = grid.dijkstras_algorithm((4, 4), (11, 10))
	print(grid.to_ascii(path))
