import heapq
import termcolor
import random
import time

class Grid:

	def __init__(self, grid_costs):
		self.height = len(grid_costs)
		self.width = len(grid_costs[0])
		self.grid_costs = grid_costs

	def dijkstras_algorithm(
		self, start, end, heuristic=None, diagnostic_reporting=False):
		if not self._is_coord_traversable(start):
			raise ValueError("The start coord is invalid")

		elif not self._is_coord_traversable(end):
			raise ValueError("The end coord is invalid")

		costs_so_far = {start: 0}
		came_from = {start: None}
		frontier = [(0, start)]

		if diagnostic_reporting:
			num_nodes_visited = 0

		while frontier:
			if diagnostic_reporting:
				num_nodes_visited += 1

			_, curr_coords = heapq.heappop(frontier)
			curr_cost = costs_so_far[curr_coords]
			if curr_coords == end:
				break

			for neighbor_coords in self._neighbor_coords(curr_coords):
				x, y = neighbor_coords
				neighbor_cost = curr_cost + self.grid_costs[y][x]

				if neighbor_coords not in costs_so_far or \
					neighbor_cost < costs_so_far[neighbor_coords]:
					costs_so_far[neighbor_coords] = neighbor_cost
					came_from[neighbor_coords] = curr_coords
					priority = neighbor_cost

					if heuristic is not None:
						priority += heuristic(neighbor_coords)

					heapq.heappush(frontier, (priority, neighbor_coords))

		else:
			return None

		if diagnostic_reporting:
			print("Number of visited nodes:", num_nodes_visited)

		path = []
		while curr_coords is not None:
			path.append(curr_coords)
			curr_coords = came_from[curr_coords]
		path.reverse()

		return path

	def astar(self, start, end, diagnostic_reporting=False):
		def manhattan_distance(curr_coords):
			return abs(end[0] - curr_coords[0]) + abs(end[1] - curr_coords[1])

		return self.dijkstras_algorithm(
			start, end, manhattan_distance, diagnostic_reporting)

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
		1 1 _ 1 _ 1 _ 1 _ 5 7 1
		1 1 _ 1 1 1 _ 1 _ 1 1 1""")
	start = (4, 4)
	end = (11, 10)
	path = grid.dijkstras_algorithm(start, end)
	print("Path from {} to {} using Dijkstra's: {}".format(start, end, path))
	print(grid.to_ascii(path), "\n")

	path = grid.astar(start, end)
	print("Path from {} to {} using A*: {}".format(start, end, path))
	print(grid.to_ascii(path))

	unreachable_location = (7, 10)
	print("\nPath to {} (an unreachable location): {}".format(
		unreachable_location,
		grid.dijkstras_algorithm((4, 4), unreachable_location)))

	grid = Grid.from_ascii("""
	 	 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
	 	 1 1 1 1 1 _ _ _ _ _ _ _ _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 _ 1 1
	 	 1 1 _ _ _ _ _ _ _ _ _ _ _ 1 1
	 	 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
	 	 """)

	print(
		"\nComparing the number of nodes visited by Dijkstra's and A* for the "
		"following grid:")
	print(grid.to_ascii())

	start = (3, 9)
	end = (14, 0)

	print("Dijkstra's:")
	grid.dijkstras_algorithm(start, end, diagnostic_reporting=True)

	print("A*:")
	grid.astar(start, end, diagnostic_reporting=True)

	grid_size = 100
	print("\nTesting performance for a {0}x{0} grid:".format(grid_size))
	random_grid_costs = [
		[random.choice([1, 1, 1, 2]) if random.randint(1, 5) < 5 else None
			for _ in range(grid_size)]
		for _ in range(grid_size)]

	start = (0, 0)
	end = (grid_size - 1,) * 2

	for x, y in [start, end]:
		random_grid_costs[y][x] = 1

	random_grid = Grid(random_grid_costs)

	def timeit(algorithm_name, pathfinding_func):
		start_time = time.time()
		pathfinding_func(start, end, diagnostic_reporting=True)
		time_diff = time.time() - start_time
		print("{} took {}s".format(algorithm_name, time_diff))

	timeit("Dijkstra's", random_grid.dijkstras_algorithm)
	print("")
	timeit("A*", random_grid.astar)
