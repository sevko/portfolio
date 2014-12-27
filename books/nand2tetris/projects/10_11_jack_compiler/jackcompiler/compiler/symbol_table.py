"""
Manages variable symbols for a given scope.
"""

import collections

Symbol = collections.namedtuple("Symbol", ["name", "segment", "kind", "index"])

class SymbolTable(object):
	"""

	Attributes:
		SYMBOL_TYPES (list of str): The variable qualifiers allowed to be
			stored in this table.
		SYMBOL_TYPE_SEGMENTS (dict): Maps variable qualifies (see
			`SYMBOL_TYPES`) to Jack Virtual Machine memory segments.
		class_name (str): The name of the current class.
		label_uid (int): Tracks the UID used to distinguihs labels from one
			another.
		_table (dict): Maps symbol names to `Symbol` objects.
		_indexes (dict): Maps qualifier names (see `SYMBOL_TYPES`) to the
			number of existing symbols of that type. Used to provide new
			symbols with unique indexes in their qualifier segment.
	"""

	SYMBOL_TYPES = ["static", "field", "argument", "var"]
	SYMBOL_TYPE_SEGMENTS = {
		"static": "static",
		"argument": "argument",
		"var": "local",
		"field": "this"
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
