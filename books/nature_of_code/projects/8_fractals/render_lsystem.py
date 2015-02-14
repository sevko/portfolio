"""
A sketch of a generative L-system.
"""

rules = None
curr_gen = None
length = None
theta = None

def setup():
	global rules, curr_gen, length, theta

	size(1000, 600)
	rules = {
		"F": "FF+[+F-F-F]-[-F+F+F]"
	}
	curr_gen = ["F"]
	length = width / 8
	theta = radians(25)
	noLoop()

def mouseClicked():
	redraw()

def draw():
	"""
	Iterate through another generation of the L-system.
	"""

	global curr_gen, counter, length

	background(255)
	textSize(14)
	fill(0)
	text(
		"Click the mouse to iterate through another generation.",
		width * 0.1, height * 0.1
	)

	translate(width / 2, height / 2)
	next_gen = []
	for member in curr_gen:
		next_gen.append(rules[member] if member in rules else member)
		render_member(member)

	length *= 0.5
	curr_gen = "".join(next_gen)
	redraw()

def render_member(member):
	"""
	Render a member of the L-system generation in the "F+-[]" alphabet.
	"""

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
