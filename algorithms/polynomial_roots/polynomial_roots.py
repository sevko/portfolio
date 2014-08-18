def polynomial_root(polynomial, derivative, x, max_delta=1e-6):
	prev_x = x - 2 * max_delta # So as to fail the while-loop conditional.
	while max_delta < abs(x - prev_x):
		prev_x = x
		x -= polynomial(x) / derivative(x)

	return x
