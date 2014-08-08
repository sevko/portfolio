"""
An implementation of Newtonian polynomial interpolation.
"""

def interpolate_polynomial_value(known_points, value):
	"""
	Interpolate a polynomial value.

	Given a set of points on a polynomial curve and the x-coordinate of a
	certain point, use Newtonian polynomial interpolation to approximate the
	y-coordinate of that point.

	Args:
		known_points (list of tuples of floats): A set of points that lie on a
			given polynomial curve. The points are represented as tuples of two
			floats (x and y coordinates) inside a list, ordered by ascending
			x-coordinate value.
		value (float): The x-coordinate of the point whose y-coordinate will be
			approximated.

	Returns:
		The interpolated y-coordinate corresponding to the `value` x-coordinate
		of a point on the curve described by `known_points`.
	"""

	num_points = len(known_points)
	divided_differences = [[point[1] for point in known_points]]

	for order in xrange(1, num_points):
		divided_differences.append([])
		for difference in xrange(num_points - order):
			numerator = (divided_differences[order - 1][difference + 1] -
				divided_differences[order - 1][difference])
			denominator = (known_points[difference + order][0] -
					known_points[difference][0])
			divided_differences[order].append(numerator / denominator)

	interpolating_coefs = [order[0] for order in divided_differences]
	interpolated_value = 0
	for order in xrange(num_points):
		order_val = interpolating_coefs[order]
		for multiplier in xrange(order):
			order_val *= (value - known_points[multiplier][0])
		interpolated_value += order_val

	return interpolated_value

def test_polynomial_interpolation():
	"""
	Test `interpolate_polynomial_value()`.

	`assert` the output of `interpolate_polynomial_value()` against handwritten
	interpolated values.
	"""

	poly_points = [
		(-3.0, -5.0),
		(-2.0, -1.1),
		(2.0, 1.9),
		(3.0, 4.8)
	]

	test_values = [
		(-2.5, -2.69375),
		(0.0, 0.8),
		(2.5, 3.04375)
	]

	for test in test_values:
		# Floating point comparison with a small error allowance.
		assert (
			abs(interpolate_polynomial_value(poly_points, test[0]) - test[1]) <
			1e-6
		)
