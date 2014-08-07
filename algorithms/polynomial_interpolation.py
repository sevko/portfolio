def interpolate_polynomial_value(known_points, value):
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

def test_polynomial_interpolation():
	interpolate_polynomial_value(
		[(-3.0, -5.0), (-2.0, -1.1), (2.0, 1.9), (3.0, 4.8)], 10
	)

if __name__ == "__main__":
	test_polynomial_interpolation()
