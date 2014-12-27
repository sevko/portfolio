"""
Classes representing the Hack Virtual Machine's arithmetic/logic binary/unary
operations.
"""

from vmtranslator.operations import operation
import abc

class ALOperation(operation.Operation, metaclass=abc.ABCMeta):
	"""
	An arithmetic and logic operation.
	"""

	OPERATION_STRING = {}

	def __init__(self, op_string):
		self._operation = self.OPERATION_STRING[op_string]

	@classmethod
	def from_string(cls, string, state):
		for oper in cls.OPERATION_STRING:
			if string == oper:
				return cls(string)

class BinaryOp(ALOperation):
	"""
	A binary arithmetic/bitwise operation.
	"""

	OPERATION_STRING = {
		"add": "+",
		"sub": "-",
		"and": "&",
		"or": "|"
	}

	def to_assembly(self):
		asm = """@SP
			A = M - 1
			D = M
			@SP
			M = M - 1
			A = M - 1
			M = M {0} D"""

		return asm.format(self._operation)

class UnaryOp(ALOperation):
	"""
	A unary arithmetic/logic operation.
	"""

	OPERATION_STRING = {
		"not": "!",
		"neg": "-"
	}

	def to_assembly(self):
		asm = """@SP
			A = M - 1
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

	def __init__(self, op_string, uid):
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

		super().__init__(op_string)
		self._uid = uid

	def to_assembly(self):
		asm = """@SP
			A = M - 1
			D = M

			@diff
			M = D

			@SP
			M = M - 1
			A = M - 1
			D = M

			@diff
			D = D - M

			@TRUE_{1}
			D; {0}
			D = 0

			@END_{1}
			0;JMP

			(TRUE_{1})
			D = -1

			(END_{1})
			@SP
			A = M - 1
			M = D"""

		return asm.format(self._operation, self._uid)

	@classmethod
	def from_string(cls, string, state):
		for oper in cls.OPERATION_STRING:
			if string == oper:
				state["num logic ops"] += 1
				return cls(string, state["num logic ops"])

OPS = [BinaryOp, UnaryOp, LogicOp]
