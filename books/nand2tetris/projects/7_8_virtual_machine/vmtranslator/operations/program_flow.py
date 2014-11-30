import re

from vmtranslator.operations import operation

class ProgramFlowOp(operation.Operation):

	LABEL_REGEX = "[{0}][{0}0-9]*".format("a-zA-Z_.:")

	def __init__(self, function_name, label):
		self._label = "{0}${1}".format(function_name, label)

	@classmethod
	def from_string(cls, string, state):
		op_regex = "({0}) ({1})".format(cls.OP_STRING, cls.LABEL_REGEX)
		match = re.match(op_regex, string)
		if match:
			return cls(state["function name"], match.groups()[1])

class GotoOp(ProgramFlowOp):

	OP_STRING = "goto"

	def to_assembly(self):
		return """@{0}
			0;JMP
			""".format(self._label)

class IfGotoOp(ProgramFlowOp):

	OP_STRING = "if-goto"

	def to_assembly(self):
		return """@SP
			M = M - 1
			A = M
			D = M
			@{0}
			!D; JEQ
			""".format(self._label)

class LabelOp(ProgramFlowOp):

	OP_STRING = "label"

	def to_assembly(self):
		return "({0})\n".format(self._label)

ops = [GotoOp, IfGotoOp, LabelOp]
