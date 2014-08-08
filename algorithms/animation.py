from matplotlib import animation, pyplot
import numpy

import polynomial_interpolation

def animate():
	def init():
		# Draw the background of every frame.

		line.set_data([], [])
		points.set_data([], [])
		return line, points

	def animate(i):
		# Animation function.

		x = numpy.linspace(-4, 4, 10)
		y = 0.1 * (x ** 3 + 0.4 * x ** 2 + x)
		line.set_data(x, y)
		points.set_data(x, y)
		return line, points

	figure = pyplot.figure()
	axes = pyplot.axes(xlim=(-4, 4), ylim=(-4, 4))
	line, = axes.plot([], [], linewidth=2)
	points, = axes.plot([], [], "ro", markersize=4)
	anim = animation.FuncAnimation(
		figure, animate, init_func=init, frames=200, interval=20, blit=True
	)
	pyplot.show()

if __name__ == "__main__":
	animate()
