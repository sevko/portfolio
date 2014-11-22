class SymbolTable(object):

	VARIABLE_OFFSET = 16

	def __init__(self):
		self.num_variables = 0
		self.table = {}

	def add_variable(self, variable):
		address = self.VARIABLE_OFFSET + self.num_variables
		self.num_variables += 1
		self.table[variable] = address
		return address

	def add_label(self, label, instr_num):
		self.table[label] = instr_num

	def __getitem__(self, symbol):
		return self.table[symbol]
