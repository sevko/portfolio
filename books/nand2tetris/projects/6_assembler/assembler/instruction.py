import re

from assembler import parser

def parse_instruction(string):
	return CInstr.from_string(string) or AInstr.from_string(string)

class AInstr(object):

	def __init__(self, symbol=None, value=None):
		if symbol is None and value is None:
			raise Exception(
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
			raise Exception(
				"TODO: better message. Either a dest or jump needed."
			)
		self.dest = dest
		self.comp = comp
		self.jump = jump

	def to_binary(self):
		comp_binary = {
			"0": "0101010",
			"1": "0111111",
			"-1": "0111010",
			"D": "0001100",
			"A": "0110000",
			"!D": "0001101",
			"!A": "0110001",
			"-D": "0001111",
			"-A": "0110011",
			"D+1": "0011111",
			"A+1": "0110111",
			"D-1": "0001110",
			"A-1": "0110010",
			"D+A": "0000010",
			"D-A": "0010011",
			"A-D": "0000111",
			"D&A": "0000000",
			"D|A": "0010101",
			"M": "1110000",
			"!M": "1110001",
			"-M": "1110011",
			"M+1": "1110111",
			"M-1": "1110010",
			"D+M": "1000010",
			"D-M": "1010011",
			"M-D": "1000111",
			"D&M": "1000000",
			"D|M": "1010101"
		}

		dest_binary = {
			None: "000",
			"M":    "001",
			"D":    "010",
			"MD":   "011",
			"A":    "100",
			"AM":   "101",
			"AD":   "110",
			"AMD":  "111"
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
