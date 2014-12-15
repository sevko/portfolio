import collections

Symbol = collections.namedtuple("Symbol", ["name", "type_", "kind", "index"])

class SymbolTable(object):

	SYMBOL_TYPES = ["static", "field", "argument", "var"]

	def __init__(self):
		self._table = {}
		self._indexes = {type_: 0 for type_ in self.SYMBOL_TYPES}

	def __contains__(self, item):
		return item in self._table

	def __getitem__(self, key):
		return self._table[key]

	def clear_symbols(self):
		self._table = {}

	def add_symbol(self, name, type_, kind):
		self._table[name] = Symbol(name, type_, kind, self._indexes[kind])
		self._indexes[kind] += 1
