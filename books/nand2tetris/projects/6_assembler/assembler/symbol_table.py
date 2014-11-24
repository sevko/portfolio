"""
An implementation of a simple symbol table, to allow the use of variables and
labels in A instructions.
"""

class SymbolTable(object):
	"""

	Attributes:
		num_variables (number): The number of variables stored in the symbol
			table; used to keep track of the memory address for the next
			variable.
		table (dictionary): A dictionary mapping label/variable names to their
			instruction memory/RAM addresses.
		VARIABLE_OFFSET (number): The memory offset at which the first variable
			can be stored.
	"""

	VARIABLE_OFFSET = 16

	def __init__(self):
		self.num_variables = 0
		self.table = {
			"SP": 0x0000,
			"LCL": 0x0001,
			"ARG": 0x0002,
			"THIS": 0x0003,
			"THAT": 0x0004,
			"SCREEN": 0x4000,
			"KBD": 0x6000
		}

		for var in xrange(16):
			self.table["R{0}".format(var)] = var

	def add_variable(self, variable):
		"""
		Adds a variable to this table, incrementing `.num_variables`.

		Args:
			variable (string): The name of the variable.

		Returns:
			(number) If `variable` does not exist in this table, add it. Then,
			return the memory address mapped to `variable`.
		"""

		if variable in self.table:
			return self.table[variable]

		address = self.VARIABLE_OFFSET + self.num_variables
		self.num_variables += 1
		self.table[variable] = address
		return address

	def add_label(self, label, instr_num):
		"""
		Args:
			label (string): The label name.
			instr_num (number): The address in instruction memory to map the
				label to.
		"""

		self.table[label] = instr_num

	def __getitem__(self, symbol):
		return self.table[symbol]
