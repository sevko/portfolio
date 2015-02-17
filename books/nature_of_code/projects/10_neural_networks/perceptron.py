import random

class Perceptron(object):

	def __init__(self, num_weights, learning_constant=1):
		self.weights = [random.uniform(-1, 1) for _ in range(num_weights)]
		self.learning_constant = learning_constant

	def feedforward(self, inputs):
		if len(inputs) != len(self.weights):
			raise Exception("Number of inputs and weights must be equal.")

		outputs = [
			input_ * weight for input_, weight in zip(inputs, self.weights)
		]
		return self._activate(sum(outputs))

	def train(self, inputs, expected):
		result = self.feedforward(inputs)
		err = expected - result
		self.weights = [
			weight + err * input_ * self.learning_constant
			for input_, weight in zip(inputs, self.weights)
		]

	def _activate(self, output):
		return 1 if output > 0 else -1

globals_ = {}

def setup():
	size(1000, 500)
	globals_["perceptron"] = Perceptron(3)
	globals_["count"] = 0
	globals_["function"] = lambda x: 0.5 * x

	test_cases = []
	for _ in range(1000):
		pt = (random.randint(0, width), random.randint(0, height), 1)
		expected = 1 if globals_["function"](pt[0]) < pt[1] else -1
		test_cases.append((pt, expected))

	globals_["test_cases"] = test_cases

def draw():
	globals_["count"] += 1
	print(globals_["count"])
	test_cases = globals_["test_cases"]
	test_case = test_cases[globals_["count"] % len(test_cases)]
	globals_["perceptron"].train(*test_case)

	background(255)
	line(0, globals_["function"](0), width, globals_["function"](width))
	for pt, _ in globals_["test_cases"]:
		if globals_["perceptron"].feedforward(pt) > 0:
			noFill()
		else:
			fill(0)

		ellipse(pt[0], pt[1], 8, 8)

	fill(0xFF, 0, 0)
	ellipse(test_case[0][0], test_case[0][1], 8, 8)
