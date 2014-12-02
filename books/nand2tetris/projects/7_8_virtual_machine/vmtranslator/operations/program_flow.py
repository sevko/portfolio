"""
Classes representing the Hack Virtual Machine's program-flow operations.
"""

import re

from vmtranslator.operations import operation

class ProgramFlowOp(operation.Operation):
	"""
	The base class for all program-flow operation classes. Leaves only the
	`to_assembly()` method to be defined.

	Attributes:
		_label (string): The globally unique label assigned to this program
			flow operation. Created by `__init__()`.
		OP_STRING (string): The string representation of this operation;
			defined by subclasses.
	"""

	LABEL_REGEX = "[{0}][{0}0-9]*".format("a-zA-Z_.:")
	OP_STRING = ""

	def __init__(self, function_name, label):
		"""
		Args:
			function_name (string): The name of the function encapsulating this
				program-flow operation..
			label (string): The name of this label; matches `LABEL_REGEX`.
		"""

		self._label = "{0}${1}".format(function_name, label)

	@classmethod
	def from_string(cls, string, state):
		op_regex = "({0}) ({1})".format(cls.OP_STRING, cls.LABEL_REGEX)
		match = re.match(op_regex, string)
		if match:
			return cls(state["function name"], match.groups()[1])

class GotoOp(ProgramFlowOp):
	"""
	A `goto` operation.
	"""

	OP_STRING = "goto"

	def to_assembly(self):
		return """@{0}
			0;JMP
			""".format(self._label)

class IfGotoOp(ProgramFlowOp):
	"""
	A `goto` operation conditional on the top value of the stack.
	"""

	OP_STRING = "if-goto"

	def to_assembly(self):
		return """@SP
			M = M - 1
			A = M
			D = M
			@{0}
			D; JNE
			""".format(self._label)

class LabelOp(ProgramFlowOp):
	"""
	A `label` operation.
	"""

	OP_STRING = "label"

	def to_assembly(self):
		return "({0})\n".format(self._label)

OPS = [GotoOp, IfGotoOp, LabelOp]
