from matplotlib import animation, pyplot
import numpy
import random

def demo(polynomial):
	"""
	Visually demonstrate the computation of a best-fit line using
	`least_squares_estimation.estimate_coefficients()`.
	"""

	bounds = [-20, 20]
	x_coors = range(bounds[0], bounds[1])
	y_coors = [polynomial(x) for x in x_coors]

	# Configure matplotlib and render the dataset/line.
	figure = pyplot.figure()
	axes = pyplot.axes(
		xlim=(bounds[0], bounds[1]),
		ylim=(min(y_coors), max(y_coors))
	)

	line, = axes.plot(x_coors, y_coors, "g-", label="polynomial", linewidth=1)
	x_axis, = axes.plot(
		[bounds[0], bounds[1]], [0, 0], "b-", label="x-axis", linewidth=1
	)
	pyplot.show()
