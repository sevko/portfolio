"""
The core of the vmtranslator package. Aggregates all sub-modules and performs
the actual compilation of VM code to assembly.
"""

from vmtranslator import parser

def translate(code):
	"""
	Parses and translates a Hack Virtual Machine program into equivalent Hack
	assembly.

	Args:
		code (string): The raw VM code, which will be filtered and parsed.

	Returns:
		(string) The entire assembly program compiled from `code`.
	"""

	clean_code = parser.clean_code(code)

	instructions = []
	state = {
		"num logic ops": 0,
		"function name": "Sys.init",
	} # Modified by `parser.parse_line().`

	for line in clean_code.split("\n"):
		parsed = parser.parse_line(line, state)
		if parsed is not None:
			instructions.append(parsed)

	return "\n".join([instr.to_assembly() for instr in instructions ])
