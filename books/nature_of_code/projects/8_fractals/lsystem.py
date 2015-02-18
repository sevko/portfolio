"""
A dead-simple L-system implementation.
"""

def iterate(curr_gen, rules):
	return "".join([rules[member] for member in curr_gen])

if __name__ == "__main__":
	curr_gen = "A"
	rules = {
		"A": "AB",
		"B": "A"
	}
	for gen in range(10):
		curr_gen = iterate(curr_gen, rules)
		print(gen, curr_gen)
