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
	num_points = 60
	pts_x = [
		float(random.randint(bounds[0], bounds[1])) for pt in
		xrange(num_points)
	]
	pts_y = [
		float(random.randint(bounds[0], bounds[1])) for pt in
		xrange(num_points)
	]

	# Create a best-fit line.
	estim = least_squares_estimation.estimate_coefficients(zip(pts_x, pts_y))
	def best_fit_line(x):
		return estim[0] * x + estim[1]

	# Configure matplotlib and render the dataset/line.
	figure = pyplot.figure()
	axes = pyplot.axes(
		xlim=(bounds[0], bounds[1]),
		ylim=(bounds[0], bounds[1])
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
