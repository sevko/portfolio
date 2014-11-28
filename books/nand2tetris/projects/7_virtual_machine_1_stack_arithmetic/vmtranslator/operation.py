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
		"or": "|",
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
