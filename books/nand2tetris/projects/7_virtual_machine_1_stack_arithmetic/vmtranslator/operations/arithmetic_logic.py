"""
Classes representing the Hack Virtual Machine's arithmetic/logic binary/unary
operations.
"""

from vmtranslator.operations import operation

logic_op_uid = 0

class ALOperation(operation.Operation):
	"""
	An arithmetic and logic operation.
	"""

	def __init__(self, operation):
		self._operation = self.OPERATION_STRING[operation]

	@classmethod
	def from_string(cls, string):
		for op in cls.OPERATION_STRING:
			if string == op:
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

	def __init__(self, operation):
		super().__init__(operation)

	def to_assembly(self):
		asm = """@SP
			A = M - 1
			D = M
			@SP
			M = M - 1
			A = M - 1
			M = D {0} M"""

		return asm.format(self._operation)

class UnaryOp(ALOperation):
	"""
	A unary arithmetic/logic operation.
	"""

	OPERATION_STRING = {
		"not": "!",
		"neg": "-"
	}

	def __init__(self, operation):
		super().__init__(operation)

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
		self._uid = uid

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

			(TRUE_{1})
			D = 1

			(END_{1})
			@SP
			M = D"""

		return asm.format(self._operation, self._uid)

	@classmethod
	def from_string(cls, string):
		for op in cls.OPERATION_STRING:
			if string == op:
				global logic_op_uid
				logic_op_uid += 1
				return cls(string, logic_op_uid)

ops = [BinaryOp, UnaryOp, LogicOp]
