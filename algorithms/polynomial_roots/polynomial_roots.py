"""
An implementation of Newton's polynomial root estimation algorithm.
"""

def polynomial_root(polynomial, derivative, x, max_delta=1e-6):
	"""
	Estimate the root of a polynomial using Newton's method.

	Args:
		polynomial (func): A function that represents the polynomial whose root
			will be found. Must take one numeric argument, and return a numeric
			value.
		derivative (func): A function that represents the derivative of
			`polynomial`. Must take one numeric argument, and return a numeric
			value.
		x (float): A good guess (per the standards of Newton's method) for a
			root of the polynomial, used as a seed for the algorithm.
		max_delta (float): The maximum difference between subsequent
			estimations of a root that can be attained before the estimate is
			returned as the root. Smaller values result in greater precision,
			but require a few more iterations of the algorithm.

	Returns:
		(float) An estimated root of `polynomial`, found relatively near `x`.
	"""

	prev_x = x - 2 * max_delta # So as to fail the while-loop conditional.
	while max_delta < abs(x - prev_x):
		prev_x = x
		x -= polynomial(x) / derivative(x)

	return x

if __name__ == "__main__":
	def polynomial(x):
		return x ** 3 - x ** 2 - 3 * x

	def derivative(x):
		return 3 * x ** 2 - 2 * x - 3

	root = polynomial_root(polynomial, derivative, 1.5)
	print "Estimated root: %f.\nf(%f) = %f" % (root, root, polynomial(root))
