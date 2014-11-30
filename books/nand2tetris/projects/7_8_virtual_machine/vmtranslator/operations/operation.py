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

	@classmethod
	@abc.abstractmethod
	def from_string(cls, string, state):
		"""
		Try to parse out an operation from a string.

		Args:
			string (string): A string that may contain a textual representation
				of this VM operation.
			state (dictionary): The state of the parser; modified and used by
				some overriding `from_string()` implementations.

		Returns:
			(Operation) A subclass of Operation, if one could be parsed from
			`string`; else, None.
		"""

		pass
