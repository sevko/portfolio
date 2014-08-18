import demo
import math

def polynomial_root(polynomial, derivative, x, delta=1e-6):
	pass

if __name__ == "__main__":
	def polynomial(x):
		return math.sin(x)

	def derivative(x):
		return math.cos(x)
		# return 25 * x ** 4 + 8 * x + 1

	demo.demo(polynomial)
