rules = None
curr_gen = None
length = None
theta = None

def setup():
	global rules, curr_gen, length, theta

	size(500, 400)
	rules = {
		"F": "FF+[+F-F-F]-[-F+F+F]"
	}
	curr_gen = ["F"]
	length = width / 4
	theta = radians(25)

counter = 0
def draw():
	translate(width / 2, height / 2)
	global curr_gen, counter, length

	next_gen = []
	for member in curr_gen:
		next_gen.append(rules[member] if member in rules else member)
		render_member(member)

	length *= 0.5
	curr_gen = "".join(next_gen)
	counter += 1
	if counter == 5:
		noLoop()

def render_member(member):
	if member in ("F", "G"):
		line(0, 0, length, 0)
		translate(length, 0)

	elif member == "+":
		rotate(theta)

	elif member == "-":
		rotate(-theta)

	elif member == "[":
		pushMatrix()

	elif member == "]":
		popMatrix()
