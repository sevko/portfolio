"""
Visually demonstrate least-squares estimation.
"""

from matplotlib import animation, pyplot
import numpy
import random

import least_squares_estimation

def demo():
	"""
	Visually demonstrate the computation of a best-fit line using
	`least_squares_estimation.estimate_coefficients()`.
	"""

	# Create a randomized dataset.
	bounds = [-100, 100]
	pts_x = range(bounds[0], bounds[1], 4)
	pts_y = [
		pts_x[pt] * 0.5 + (pt * 0.5) ** 2 + random.random() * 40 for pt in
		xrange(len(pts_x))
	]

	# Create a best-fit line.
	estim = least_squares_estimation.estimate_coefficients(zip(pts_x, pts_y))
	def best_fit_line(x):
		return estim[0] * x + estim[1]

	# Configure matplotlib and render the dataset/line.
	figure = pyplot.figure()
	axes = pyplot.axes(
		xlim=(min(pts_x), max(pts_x)),
		ylim=(min(pts_y), max(pts_y))
	)
	points, = axes.plot(
		pts_x, pts_y, "ro", label="randomized dataset",
		markersize=8
	)

	line, = axes.plot(
		[bounds[0], bounds[1]],
		[best_fit_line(bounds[0]), best_fit_line(bounds[1])], "g-",
		label="best-fit line", linewidth=1.2
	)
	pyplot.show()

if __name__ == "__main__":
	demo()
