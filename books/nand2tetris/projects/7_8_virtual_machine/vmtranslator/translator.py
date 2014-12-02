"""
The core of the vmtranslator package. Aggregates all sub-modules and performs
the actual compilation of VM code to assembly.
"""

from vmtranslator import parser
from vmtranslator import operations

def create_bootstrap_assembly():
	setSP = """@256
		D = A
		@SP
		M = D
		"""

	callSysInit = operations.function_calling.CallFuncOp("Sys.init", 0, 0)
	return setSP + callSysInit.to_assembly()

def translate(code, state=None):
	"""
	Parses and translates a Hack Virtual Machine program into equivalent Hack
	assembly.

	Args:
		code (string): The raw VM code, which will be filtered and parsed.
		state (dictionary, optional): A dictionary used to maintain the state
			of the parser; useful when state must be persistent across the
			translation of multiple files. If omitted, a default beginning
			state will be created.

	Returns:
		(string) The entire assembly program compiled from `code`.
	"""

	instructions = []

	if state is None:
		state = {
			"num logic ops": 0,
			"function name": None,
			"function call uid": {}
		} # Updated by `parser.parse_line().`

	for line in parser.get_lines(code):
		parsed = parser.parse_line(line, state)
		if parsed is not None:
			instructions.append(parsed)
			print(
				type(parsed).__str__(parsed).split(".")[-1].split()[0].ljust(20, ' '),
				str(parsed.__dict__).ljust(60, ' '),
				line)
			# print(type(parsed).__str__(parsed).split(".")[-1].split()[0].ljust(20, ' ') + line)

	return "\n".join([instr.to_assembly() for instr in instructions])
