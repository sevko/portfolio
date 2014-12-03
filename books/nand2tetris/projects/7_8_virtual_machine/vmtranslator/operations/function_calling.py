import re

from vmtranslator.operations import operation

FUNC_NAME_REGEX = "[{0}][{0}0-9]*".format("a-zA-Z_.:")

def _parse_string(string, op_string):
	regex = "{0} ({1}) (\d+)".format(op_string, FUNC_NAME_REGEX)
	match = re.match(regex, string)
	if match:
		name, num = match.groups()
		return name, int(num)

class DeclareFuncOp(operation.Operation):

	OP_STRING = "function"

	def __init__(self, name, num_vars):
		self._name = name
		self._num_vars = num_vars

	def to_assembly(self):
		push_0 = """@SP
			D = M
			M = M + 1
			A = D
			M = 0
			"""

		return """({0})
			{1}
			""".format(self._name, push_0 * self._num_vars)

	@classmethod
	def from_string(cls, string, state):
		components = _parse_string(string, cls.OP_STRING)
		if components is not None:
			name, num_vars = components
			oper = cls(name, num_vars)
			state["function name"] = name
			return oper

class CallFuncOp(operation.Operation):

	OP_STRING = "call"

	def __init__(self, name, num_args, uid):
		self._name = name
		self._num_args = num_args
		self._uid = uid

	def to_assembly(self):
		def push_reg_value(register_name):
			return """@{0}
				D = M
				@SP
				M = M + 1
				A = M - 1
				M = D
				""".format(register_name)

		unique_label = "{0}$call${1}".format(self._name, self._uid)
		asm = """
			// Push return value
			@{0}
			D = A
			@SP
			M = M + 1
			A = M - 1
			M = D

			// LCL
			{1}

			// ARG
			{2}

			// THIS
			{3}

			// THAT
			{4}

			@SP
			D = M
			@LCL
			M = D
			@{5}
			D = D - A
			@ARG
			M = D

			@{6}
			0;JMP

			({0})
			"""

		return asm.format(
			unique_label, push_reg_value("LCL"), push_reg_value("ARG"),
			push_reg_value("THIS"), push_reg_value("THAT"), 5 + self._num_args,
			self._name
		)

	@classmethod
	def from_string(cls, string, state):
		components = _parse_string(string, cls.OP_STRING)
		if components is not None:
			name, num_args = components
			if name not in state["function call uid"]:
				state["function call uid"][name] = 0
			state["function call uid"][name] += 1
			return cls(name, num_args, state["function call uid"][name])

class ReturnOp(operation.Operation):

	def to_assembly(self):

		def pop_pointer_value(dest_reg_name):
			return """
				@frame
				M = M - 1
				A = M
				D = M
				@{0}
				M = D
				""".format(dest_reg_name)

		asm = """
			//FRAME = LCL
			@LCL
			D = M
			@frame
			M = D

			// ret = *(frame - 5)
			@5
			D = D - A
			A = D
			D = M
			@return_address
			M = D

			@SP
			M = M - 1
			A = M
			D = M
			@ARG
			A = M
			M = D
			// *ARG = pop()

			@ARG
			D = M
			@SP
			M = D + 1
			// SP = ARG + 1

			// THAT = *(FRAME - 1)
			{0}

			// THIS = *(FRAME - 2)
			{1}

			// ARG = *(FRAME - 3)
			{2}

			// LCL = *(FRAME - 4)
			{3}

			// goto RET
			@return_address
			A = M
			0;JMP
			"""

		return asm.format(
			pop_pointer_value("THAT"),
			pop_pointer_value("THIS"),
			pop_pointer_value("ARG"),
			pop_pointer_value("LCL")
		)

	@classmethod
	def from_string(cls, line, state):
		if line == "return":
			return cls()

OPS = [DeclareFuncOp, CallFuncOp, ReturnOp]
