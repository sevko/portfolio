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
