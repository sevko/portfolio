"""
The core of the assembler library; aggregates all of its other components.
"""

from assembler import instruction
from assembler import parser
from assembler import symbol_table

def assemble_code(code):
	"""
	Assemble an assembly string to machine instructions.

	Args:
		code (string): The code to assemble.

	Raises:
		Exception: Thrown when an unrecognized assembly instruction is
			encountered.

	Yields:
		(strings) All of the stringified, binary machine instructions, as
			compiled from `code`.
	"""

	symbols = symbol_table.SymbolTable()
	instructions = []

	for line in code.split("\n"):
		clean_line = parser.clean(line)

		# Skip empty lines.
		if not clean_line:
			continue

		label = parser.parse_label(clean_line)
		if label:
			symbols.add_label(label, len(instructions))
			continue

		instr = instruction.parse_instruction(clean_line)
		if instr:
			instructions.append(instr)
		else:
			raise Exception("Parse error: `{0}`".format(clean_line))

	for instr in instructions:
		if type(instr) is instruction.AInstr:
			instr.resolve(symbols)
		yield instr.to_binary()
