"""
Render an animation of polynomial interpolation with the
`polynomial_interpolation` module and `matplotlib`.
"""

from matplotlib import animation, pyplot
import numpy

import polynomial_interpolation

def animate():
	"""
	Render a visual demonstration of polynomial interpolation.

	Use `matplotlib` to render an animation of polynomial interpolation using
	the `polynomial_interpolation` module.
	"""

	import math
	domain = [-2 * math.pi, 2 * math.pi]

	def reset_frame():
		"""
		Reset the visual buffer at the beginning of a new frame.

		Returns:
			(tuple of `matplotlib` objects) A tuple of the shapes rendered by
			`matplotlib`.
		"""

		line.set_data([], [])
		points.set_data([], [])
		interpolated_line.set_data([], [])
		return line, points, interpolated_line

	def render_frame(frame):
		"""
		Render a frame of the animation.

		Args:
			frame (int): The number of the frame.

		Returns:
			(tuple of `matplotlib` objects) A tuple of the shapes rendered by
			`matplotlib`.
		"""

		# Render the full curve.
		line_domain = numpy.linspace(domain[0], domain[1], 1000)
		line.set_data(line_domain, polynomial(line_domain))

		# Render points on the curve.
		point_domain = numpy.linspace(domain[0], domain[1], frame + 1)
		point_range = polynomial(point_domain)
		points.set_data(point_domain, point_range)

		# Render the interpolated curve.
		interpolated_domain = numpy.linspace(domain[0], domain[1], 60)
		interpolated_line.set_data(
			interpolated_domain,
			polynomial_interpolation.interpolate_polynomial_value(
				zip(point_domain, point_range),
				interpolated_domain
			)
		)
		return line, points, interpolated_line

	def polynomial(x):
		"""
		The polynomial this animation is based on.

		The polynomial is alternating, 9th degree.

		Args:
			x (int/float): The argument to the polynomial.

		Returns:
			The output given by an input of `x`.
		"""

		return numpy.sin(x)
		# y = 0
		# for order in xrange(10):
			# y += (1 if order % 2 == 0 else -1) * x ** order
		# return y

	# Configure `pyplot`.
	figure = pyplot.figure()
	axes = pyplot.axes(
		xlim=(domain[0] * 1.1, domain[1] * 1.1),
		ylim=(-1.3, 1.3)
		# ylim=(polynomial(domain[0]) * 1.1, polynomial(domain[1]) * 1.1)
	)
	line, = axes.plot([], [], label="f(x)", linewidth=1.2)
	points, = axes.plot([], [], "ro", label="points on f(x)", markersize=8)
	interpolated_line, = axes.plot(
		[], [], "g-", label="interpolated f(x)", linewidth=1.2
	)
	pyplot.legend(loc="lower right")
	pyplot.title("Polynomial Interpolation")

	# Run the animation and save it to an `mp4`.
	anim = animation.FuncAnimation(
		figure, render_frame, init_func=reset_frame, frames=12, interval=1000,
		blit=True
	)
	anim.save(
		"animation/interpolation2.mp4", fps=1,
		extra_args=["-vcodec", "libx264"]
	)
	pyplot.show()

if __name__ == "__main__":
	# animate()
	polynomial_interpolation.test_polynomial_interpolation()
