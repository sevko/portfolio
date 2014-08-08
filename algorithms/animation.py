from matplotlib import animation, pyplot
import numpy

import polynomial_interpolation

def animate():
	domain = [-4, 4]

	def init():
		# Draw the background of every frame.

		line.set_data([], [])
		points.set_data([], [])
		interpolated_point.set_data([], [])
		return line, points, interpolated_point

	def animate(i):
		# Animation function.

		line_x = numpy.linspace(domain[0], domain[1], 1000)
		line_y = polynomial(line_x)
		line.set_data(line_x, line_y)

		point_x = numpy.linspace(domain[0], domain[1], i + 2)
		point_y = polynomial(point_x)
		points.set_data(point_x, point_y)

		interpolated_x = numpy.linspace(domain[0], domain[1], 40)
		interpolated_y = polynomial_interpolation.\
			interpolate_polynomial_value(zip(point_x, point_y), interpolated_x)
		interpolated_point.set_data(interpolated_x, interpolated_y)
		return line, points, interpolated_point

	def polynomial(x):
		y = 0
		for order in xrange(10):
			y += (1 if order % 2 == 0 else -1) * x ** order
		return 0.5 * y

	figure = pyplot.figure()
	axes = pyplot.axes(
		xlim=(domain[0], domain[1]),
		ylim=(polynomial(domain[0]), polynomial(domain[1]))
	)
	line, = axes.plot([], [], linewidth=1)
	points, = axes.plot([], [], "ro", markersize=8)
	interpolated_point, = axes.plot([], [], "go", markersize=4)
	anim = animation.FuncAnimation(
		figure, animate, init_func=init, frames=200, interval=1000, blit=True
	)
	pyplot.show()

if __name__ == "__main__":
	animate()
