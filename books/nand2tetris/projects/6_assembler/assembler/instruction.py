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

		return "C ({0}): {1}, {2}, {3}.".format(
			"JMP" if self.jump is not None else "INS",
			self.dest, self.comp, self.jump
		)

	@classmethod
	def from_string(cls, string):
		has_dest = string.find("=") > -1
		has_jump = string.find(";") > -1

		if has_dest or has_jump:
			dest, string = string.split("=") if has_dest else None, string
			comp, jump = string.split(";") if has_jump else string, None

			return cls(comp, dest=dest, jump=jump)
