"""
Classes representing the Hack Virtual Machine's memory manipulation operations.
"""

import re
from vmtranslator.operations import operation

class MemoryOp(operation.Operation):
	"""
	A memory operation.

	Attributes:
		_segment (string): The segment name, as parsed from a VM operation.
		_index (number): The segment index, as parsed from a VM operation.
		STATIC_SEGMENTS (dictionary): Maps "static" segment names (see
			`_segment`) to their exact addresses in memory, which allows
			precomputing addresses via `_get_static_address()`.
		DYNAMIC_SEGMENTS (dictionary): Maps "dynamic" segment names to the
			pre-defined assembly symbols, which are translated to the memory
			locations that contain the base address of the desired segment.

		OP_STRING (string): Defined by subclasses; the string representation of
			this operation (eg, "push", "pop").
	"""

	DYNAMIC_SEGMENTS = {
		"argument": "ARG",
		"local": "LCL",
		"this": "THIS",
		"that": "THAT"
	}

	STATIC_SEGMENTS = {
		"constant": 0,
		"pointer": 3,
		"temp": 5,
		"static": 16
	}

	def __init__(self, segment, index):
		self._segment = segment
		self._index = index

	def _get_static_address(self):
		"""
		Should only be called if `self._segment` is in `self.STATIC_SEGMENTS`.

		Returns:
			The target address of this memory operation.
		"""

		return self.STATIC_SEGMENTS[self._segment] + self._index

	@classmethod
	def from_string(cls, string, state):
		parts = string.split(" ")
		if len(parts) == 3:
			op, segment, index = parts
			if op == cls.OP_STRING and \
				(segment in cls.STATIC_SEGMENTS or \
				segment in cls.DYNAMIC_SEGMENTS):
				return cls(segment, int(index))

class PushOp(MemoryOp):
	"""
	Push a value from memory onto the stack.
	"""

	OP_STRING = "push"

	def to_assembly(self):
		asm = """@SP
			A = M
			M = D
			@SP
			M = M + 1
			"""

		if self._segment in self.DYNAMIC_SEGMENTS:
			base_address_asm = """@{0}
				D = M
				@{1}
				A = A + D
				D = M
				""".format(self.DYNAMIC_SEGMENTS[self._segment], self._index)

			return base_address_asm + asm

		elif self._segment in self.STATIC_SEGMENTS:
			if self._segment == "constant":
				return """@{0}
					D = A
					{1}""".format(self._index, asm)

			return """@{0}
				D = M
				{1}
				""".format(self._get_static_address(), asm)

class PopOp(MemoryOp):
	"""
	Pop a value from the stack and into a part of memory.
	"""

	OP_STRING = "pop"

	def to_assembly(self):
		asm = """
			@SP
			A = M - 1
			D = M
			@SP
			M = M - 1
			@{0}
			M = D
			"""

		if self._segment in self.DYNAMIC_SEGMENTS:
			base_address_asm = """@{0}
				D = M
				@pop_address
				M = D
				@{1}
				D = A
				@pop_address
				M = M + D
				""".format(
					self.DYNAMIC_SEGMENTS[self._segment], self._index
				)

			return base_address_asm + asm.format("pop_address\nA = M")

		elif self._segment in self.STATIC_SEGMENTS:
			if self._segment == "constant":
				return

			base_address = self._get_static_address()
			return asm.format(base_address)

ops = [PushOp, PopOp]
