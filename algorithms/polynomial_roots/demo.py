from matplotlib import animation, pyplot
import numpy

import polynomial_roots

def demo(polynomial):
	domain = [-3, 3]
	x_coors = numpy.linspace(domain[0], domain[1], 100)
	y_coors = polynomial(x_coors)

	# Configure matplotlib and render everything.
	figure = pyplot.figure()
	axes = pyplot.axes(
		xlim=(domain[0], domain[1]), ylim=(min(y_coors), max(y_coors))
	)

	line, = axes.plot(x_coors, y_coors, "g-", label="polynomial", linewidth=1)
	x_axis, = axes.plot(
		[domain[0], domain[1]], [0, 0], "b-", label="x-axis", linewidth=1
	)
	pyplot.show()

if __name__ == "__main__":
	def polynomial(x):
		return x ** 3 - x ** 2 - 3 * x

	def derivative(x):
		return 3 * x ** 2 - 2 * x - 3

	demo(polynomial)
