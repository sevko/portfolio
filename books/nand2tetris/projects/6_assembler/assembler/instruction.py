import re

from assembler import parser

def parse_instruction(string):
	return CInstr.from_string(string) or AInstr.from_string(string)

class AInstr(object):

	def __init__(self, symbol=None, value=None):
		if symbol is None and value is None:
			raise parser.AssemblerError(
				"TODO: better message. Either a symbol or value needed."
			)

		self.symbol = symbol
		self.value = value

	def resolve(self, symbol_table):
		if self.value is None:
			self.value = symbol_table.add_variable(self.symbol)

	def to_binary(self):
		return bin(self.value)[2:].zfill(16)

	@classmethod
	def from_string(cls, string):
		if string[0] == "@":
			string = string.lstrip("@")

			match = re.match("[0-9]+$", string)
			if match:
				return cls(value=int(match.group()))

			match = re.match("{0}$".format(parser.SYMBOL_REGEX), string)
			if match:
				return cls(symbol=match.group())

class CInstr(object):

	def __init__(self, comp, dest=None, jump=None):
		if dest is None and jump is None:
			raise parser.AssemblerError(
				"C instruction without either a `dest` or `jump`."
			)

		self.dest = dest
		self.comp = comp
		self.jump = jump

	def to_binary(self):
		comp_binary = {
			"0": "101010",
			"1": "111111",
			"-1": "111010",
			"D": "001100",
			"!D": "001101",
			"-D": "001111",
			"D+1": "011111",
			"D-1": "001110",
			"M": "110000",
			"!M": "110001",
			"-M": "110011",
			"M+1": "110111",
			"M-1": "110010",
			"D+M": "000010",
			"D-M": "010011",
			"M-D": "000111",
			"D&M": "000000",
			"D|M": "010101"
		}

		for comp, code in comp_binary.items():
			if comp.find("M") > -1:
				a_comp = comp.replace("M", "A")
				a_code = "0" + code

				code = "1" + code
				comp_binary[a_comp] = a_code
				comp_binary[comp] = code

				if comp.find("+") > -1:
					for alt_comp, alt_code in (comp, code), (a_comp, a_code):
						comp_binary[alt_comp[::-1]] = alt_code
			else:
				comp_binary[comp] = "0" + code

		dest_binary = {
			None: "000",
			"M": "001",
			"D": "010",
			"MD": "011",
			"A": "100",
			"AM": "101",
			"AD": "110",
			"AMD": "111"
		}

		jump_binary = {
			None: "000",
			"JGT": "001",
			"JEQ": "010",
			"JGE": "011",
			"JLT": "100",
			"JNE": "101",
			"JLE": "110",
			"JMP": "111"
		}

		return "111{0}{1}{2}".format(
			comp_binary[self.comp], dest_binary[self.dest],
			jump_binary[self.jump]
		)

	@classmethod
	def from_string(cls, string):
		has_dest = string.find("=") > -1
		has_jump = string.find(";") > -1

		if has_dest or has_jump:
			dest, string = string.split("=") if has_dest else (None, string)
			comp, jump = string.split(";") if has_jump else (string, None)

			return cls(comp, dest=dest, jump=jump)
