class SymbolTable(object):

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
		if variable in self.table:
			return self.table[variable]

		address = self.VARIABLE_OFFSET + self.num_variables
		self.num_variables += 1
		self.table[variable] = address
		return address

	def add_label(self, label, instr_num):
		self.table[label] = instr_num

	def __getitem__(self, symbol):
		return self.table[symbol]
