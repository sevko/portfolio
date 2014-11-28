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
	instructions = [parser.parse_line(line) for line in clean_code.split("\n")]
	return "\n".join([
		instr.to_assembly() for instr in instructions if instr is not None
	])
