"""
An implementation of the least-squares estimation algorithm.
"""

def estimate_coefficients(pts):
	"""
	Compute the estimators of a best-fit line for a dataset.

	Calculate the coefficient `a` and offset `b` of a line, whose function (in
	slope-intercept form), is: `y(x) = a * x + b`, using the least-squares
	estimation algorithm.

	Args:
		pts (list of tuple of 2 float): A list of two-dimensional points. Each
		tuple represents a Cartesian point, with its 0th index containing the
		point's `x` coordinate, and its 1st index containing its `y`
		coordinate.

	Returns:
		(tuple of float) A tuple in the form (a, b), where `a` and `b` are the
		estimators of `pts`'s best-fit line (see the full description of the
		function above).
	"""

	sum_x = sum_sq_x = sum_y = sum_products = 0
	num_points = len(pts)

	for pt in xrange(num_points):
		sum_x += pts[pt][0]
		sum_sq_x += pts[pt][0] ** 2
		sum_y += pts[pt][1]
		sum_products += pts[pt][0] * pts[pt][1]

	b1 = ((num_points * sum_products - sum_x * sum_y) /
		(num_points * sum_sq_x - sum_x ** 2))
	b0 = (sum_y - b1 * sum_x) / num_points

	return b1, b0

def unit_test():
	"""
	Test the output of `estimate_coefficients()` against a hand-written
	data-set and expected output.
	"""

	pts = [
		(-4.0, -3.0),
		(-3.0, -1.0),
		(-2.0, -2.0),
		(-1.5, -0.5),
		(-0.5, 1.0),
		(1.0, 0.0),
		(2.0, 1.5),
		(3.5, 1.0),
		(4.0, 2.5)
	]

	expected = (0.5519, 0.0249)
	received = estimate_coefficients(pts)
	error_allowance = 1e-6
	if abs(expected[0] - received[0]) < error_allowance or \
		abs(expected[1] - received[1]) < error_allowance:
		print "Unit-test failed.\nExpected: %s\nReceived:%s\n" % (
			expected, received
		)

if __name__ == "__main__":
	unit_test()
