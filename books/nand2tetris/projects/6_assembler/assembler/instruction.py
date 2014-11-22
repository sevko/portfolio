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
		return "A instruction: {0}".format(self.value)

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
		return "C instruction ({0}): {1}, {2}, {3}.".format(
			"JMP" if self.jump is not None else "INS",
			self.dest, self.comp, self.jump
		)

	@classmethod
	def from_string(cls, string):
		if string.find("=") > -1:
			dest, comp = string.split("=")
			return CInstr(comp, dest=dest)

		elif string.find(";") > -1:
			comp, jump = string.split(";")
			return CInstr(comp, jump=jump)
