"""
Classes representing assembly instructions and helper functions.
"""

import re

from assembler import parser

def parse_instruction(string):
	return CInstr.from_string(string) or AInstr.from_string(string)

class AInstr(object):
	"""
	An A instruction.

	Attributes:
		symbol (string): The symbol the A instruction references; needs to be
			resolved to a numeric value with `AInstr.resolve()`. See
			`AInstr.__init__` for more details on why it's necessary.
		value (int): The memory address the A instruction references. Either
			needs to be specified on instantiation, or added with
			`AInstr.resolve()`.
	"""

	def __init__(self, symbol=None, value=None):
		"""
		Args:
			symbol (string, co-optional): The symbol (either an assembly label
				or variable) the A instruction references; only needs to be
				specified if no numeric `value` (see next) can be inferenced.
			value (number, co-optional): The numeric address the instructions
				references. If `None` (the default), it must be resolved later
				with `AInstr.resolve()`, because compiled instructions need
				concrete numeric addresses.

		Raises:
			AssemblerError: If both `symbol` and `value` are `None`.
		"""

		if symbol is None and value is None:
			raise parser.AssemblerError(
				"TODO: better message. Either a symbol or value needed."
			)

		self.symbol = symbol
		self.value = value

	def resolve(self, symbol_table):
		"""
		Must be called if this `AInstr` was
		created without a `value` (thus, only with a `symbol`) before
		`AInstr.binary()` is called.

		Args:
			symbol_table (SymbolTable): The symbol table to resolve
				`self.symbol` against, to pair it with a numeric memory
				address.
		"""

		if self.value is None:
			self.value = symbol_table.add_variable(self.symbol)

	def to_binary(self):
		"""
		Returns:
			(string) A binary-string, machine code representation of the
			`AInstr`.
		"""

		return bin(self.value)[2:].zfill(16)

	@classmethod
	def from_string(cls, string):
		"""
		Args:
			string (string): The string to decode an `AInstr` from.

		Returns:
			If an AInstr could be decoded, an `AInstr` instance; otherwise,
			`None`. Note that, if the string contained an A instruction with a
			symbolic (and not numeric) reference, it'll have to be resolved
			against a symbol table with `.resolve()`.
		"""

		if string[0] == "@":
			string = string.lstrip("@")

			match = re.match("[0-9]+$", string)
			if match:
				return cls(value=int(match.group()))

			match = re.match("{0}$".format(parser.SYMBOL_REGEX), string)
			if match:
				return cls(symbol=match.group())

class CInstr(object):
	"""
	A C instruction.

	Attributes:
		comp (string or None): The operation to perform.
		dest (string or None): The destination to store the results of `comp`
			in.
		jump (string or None): The jump instruction to execute alongside
			`comp`.
	"""

	def __init__(self, comp, dest=None, jump=None):
		"""
		Args:
			See `CInstr` docstring for all identically named attributes.

		Raises:
			AssemblerError: If both `dest` and `jump` are `None`, wnich renders
				an invalid C instruction.
		"""

		if dest is None and jump is None:
			raise parser.AssemblerError(
				"C instruction without either a `dest` or `jump`."
			)

		self.dest = dest
		self.comp = comp
		self.jump = jump

	def to_binary(self):
		"""
		Returns:
			(string) A binary-string, machine code representation of this
				`CInstr`.
		"""

		# The `c` bits for most of a C instruction's `comp` operations. The `a`
		# bits are added in automatically in a follow-up loop, which also adds
		# the `A` register equivalents of each `M` register operation.
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

		# Add `a` bits to all values in `comp_binary`, and add the `A` register
		# equivalent of all `M` register operations. Also, add duplicate values
		# for all operations where the operands can be reversed (for instance,
		# `D+M` -> `D+M`, `M+D`).
		for comp, code in comp_binary.items():
			if comp.find("M") > -1:
				a_comp = comp.replace("M", "A")
				a_code = "0" + code

				code = "1" + code
				comp_binary[a_comp] = a_code
				comp_binary[comp] = code

				if re.search("[+|&]", comp):
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
		"""
		Args:
			string (string): The string to parse a `CInstr` from.

		Returns:
			A `CInstr`, if one could be decoded; otherwise, `None`.
		"""

		has_dest = string.find("=") > -1
		has_jump = string.find(";") > -1

		if has_dest or has_jump:
			dest, string = string.split("=") if has_dest else (None, string)
			comp, jump = string.split(";") if has_jump else (string, None)

			return cls(comp, dest=dest, jump=jump)
