def polynomial_root(polynomial, derivative, x, max_delta=1e-6):
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
