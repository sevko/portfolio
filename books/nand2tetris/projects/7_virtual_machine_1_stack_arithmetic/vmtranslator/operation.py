"""
Classes representing the Hack Virtual Machine's arithmetic/logic binary/unary
operations.
"""

import abc

class Operation(metaclass=abc.ABCMeta):
	"""
	The ABC for all Hack Virtual Machine arithmetic/logic operations.
	"""

	@abc.abstractmethod
	def to_assembly(self):
		"""
		Returns:
			(string) The Hack assembly instructions representing this binary
			operation.
		"""

		pass

class BinaryOp(Operation):
	"""
	A binary arithmetic/bitwise operation.
	"""

	OPERATION_STRING = {
		"add": "+",
		"sub": "-",
		"and": "&",
		"or": "|"
	}

	def __init__(self, operation):
		"""
		Args:
			operation (string): The VM language operation. Any one of:

				* "add": addition
				* "sub": subtraction
				* "and": a bitwise And (`&`)
				* "or": a bitwise Or (`|`)
		"""

		self._operation = self.OPERATION_STRING[operation]

	def to_assembly(self):
		asm = """@SP
			A = M
			D = M
			@SP
			M = M - 1
			M = D {0} M"""

		return asm.format(self._operation)

class UnaryOp(Operation):
	"""
	A unary arithmetic/logic operation.
	"""

	OPERATION_STRING = {
		"not": "!",
		"neg": "-"
	}

	def __init__(self, operation):
		"""
		Args:
			operation (string): The VM language operation. Any one of:

				* "-": Negation.
				* "Not": a bitwise Not (`!`)
		"""

		self._operation = self.OPERATION_STRING[operation]

	def to_assembly(self):
		asm = """@SP
			A = M
			M = {0} M"""

		return asm.format(self._operation)

class LogicOp(BinaryOp):
	"""
	A logic (inherently binary) operation.
	"""

	OPERATION_STRING = {
		"eq": "JEQ",
		"gt": "JGT",
		"lt": "JLT"
	}

	def __init__(self, operation, uid):
		"""
		Args:
			operation (string): VM language operation. Any one of:

				"eq": Whether operand 1 equals operand 2 (`=`).
				"gt": Whether operand 1 is greater than operand 2 (`>`).
				"lt": Whether operand 1 is less than operand 2 (`<`).

			uid (string): The unique id of this logic operation relative to all
				other logical (`LogicOp`) operations in the generated assembly
				program. Since the `LogicOp` assembly representation relies on
				labels, a uid is necessary to ensure they don't conflict with
				one another.
		"""

		self._operation = self.OPERATION_STRING[operation]
		self.uid = uid

	def to_assembly(self):
		asm = """@SP
			A = M
			D = M

			@diff
			M = D

			@SP
			AM = M - 1
			D = M

			@diff
			M = M - D

			D = M; {0}
			D = 0

			(TRUE {1})
			D = 1

			(END {1})
			@SP
			M = D"""

		return asm.format(self._operation, self.uid)

class MemoryOp(Operation):

	SEGMENTS = {
		"argument": "ARG",
		"local": "LCL",
		"static": "",
		"constant": 0,
		"this": "THIS",
		"that": "THAT",
		"pointer": "THIS",
		"temp": 5
	}

	def __init__(self, segment, index):
		pass
