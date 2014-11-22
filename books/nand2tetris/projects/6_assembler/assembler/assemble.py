from assembler import instruction
from assembler import parser
from assembler import symbol_table

def assemble_code(code):
	symbols = symbol_table.SymbolTable()
	instructions = []

	for line in code.split("\n"):
		clean_line = parser.clean(line)

		# Skip empty lines.
		if not clean_line:
			continue

		label = parser.parse_label(clean_line)
		if label:
			symbols.add_label(label, len(instructions) + 1)
			continue

		instr = instruction.parse_instruction(clean_line)
		if instr:
			instructions.append(instr)
		else:
			# raise Exception("Parse error: `{0}`".format(clean_line))
			pass

	for instr in instructions:
		if type(instr) is instruction.AInstr:
			instr.resolve(symbols)
		yield instr.to_binary()
