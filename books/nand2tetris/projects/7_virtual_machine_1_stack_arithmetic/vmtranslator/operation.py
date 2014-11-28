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

	Attributes:
		operation (string): The VM language operation string. Any one of:

			* "add": addition
			* "sub": subtraction
			* "and": a bitwise And (`&`)
			* "or": a bitwise Or (`|`)
	"""

	OPERATION_STRING = {
		"add": "+",
		"sub": "-",
		"and": "&",
		"or": "|"
	}

	def __init__(self, operation):
		self.operation = self.OPERATION_STRING[operation]

	def to_assembly(self):
		asm = """@SP
			A = M
			D = M
			@SP
			M = M - 1
			M = D {0} M"""

		return asm.format(self.operation)

class UnaryOp(Operation):
	"""
	A unary arithmetic/logic operation.

	Attributes:
		operation (string): Any one of:

			* "-": Negation.
			* "Not": a bitwise Not (`!`)
	"""

	OPERATION_STRING = {
		"Not": "!",
		"-": "-"
	}

	def __init__(self, operation):
		self.operation = operation

	def to_assembly(self):
		asm = """@SP
			A = M
			M = {0} M"""

		return asm.format(self.operation)

class LogicOp(BinaryOp):
	"""
	A logic (inherently binary) operation.

	Attributes:
		operation (string): Any one of:

			"eq": Whether operand 1 equals operand 2 (`=`).
			"gt": Whether operand 1 is greater than operand 2 (`>`).
			"lt": Whether operand 1 is less than operand 2 (`<`).

		uid (string): The unique id of this logic operation relative to all
			other logical (`LogicOp`) operations in the generated assembly
			program. Since the `LogicOp` assembly representation relies on
			labels, a uid is necessary to ensure they don't conflict with one
			another.
	"""

	def __init__(self, operation, uid):
		self.operation = operation
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

		return asm.format(self.operation, self.uid)
