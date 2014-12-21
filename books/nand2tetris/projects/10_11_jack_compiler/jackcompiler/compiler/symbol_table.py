import collections

Symbol = collections.namedtuple("Symbol", ["name", "segment", "kind", "index"])

class SymbolTable(object):

	SYMBOL_TYPES = ["static", "field", "argument", "var"]
	SYMBOL_TYPE_SEGMENTS = {
		"static": "static",
		"argument": "argument",
		"var": "local"
	}

	def __init__(self, class_name=None):
		self.class_name = class_name
		self.label_uid = 0
		self._table = {}
		self._indexes = {type_: 0 for type_ in self.SYMBOL_TYPES}

	def __contains__(self, item):
		return item in self._table

	def __getitem__(self, key):
		return self._table[key]

	def clear(self):
		self._table = {}
		self._indexes = {type_: 0 for type_ in self.SYMBOL_TYPES}

	def add_symbol(self, name, type_, kind):
		self._table[name] = Symbol(
			name, self.SYMBOL_TYPE_SEGMENTS[type_], kind, self._indexes[type_]
		)
		self._indexes[type_] += 1

	def num_symbols_by_type(self, symbol_type):
		return self._indexes[symbol_type]
