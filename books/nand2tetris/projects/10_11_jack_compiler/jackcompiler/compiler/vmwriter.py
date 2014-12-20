"""
Contains a class that writes raw Hack Virtual Machine code, abstracting away
the language with several functions.
"""

def _write_vm(function):
	def _write_vm_block(*args):
		block = function(*args)
		if block is not None:
			if args[0]._goto_marker:
				args[0]._vm_code_blocks.insert(args[0]._markers.pop(), block)
			else:
				args[0]._vm_code_blocks.append(block)

	return _write_vm_block

class VMWriter(object):

	FUNC_OP = {
		"*": "Math.multiply",
		"/": "Math.divide"
	}

	BINARY_OP = {
		"+": "add",
		"-": "sub",
		"&": "and",
		"|": "or",
		"<": "lt",
		">": "gt",
		"=": "eq"
	}

	UNARY_OP = {
		"-": "neg",
		"~": "not"
	}

	def __init__(self):
		self._vm_code_blocks = []
		self._goto_marker = False
		self._markers = []

	@_write_vm
	def push(self, segment, index):
		return "push {0} {1}".format(segment, index)

	@_write_vm
	def pop(self, segment, index):
		return "pop {0} {1}".format(segment, index)

	@_write_vm
	def binary_op(self, op):
		if op in self.FUNC_OP:
			self.call(self.FUNC_OP[op], 2)
		else:
			return self.BINARY_OP[op]

	@_write_vm
	def unary_op(self, op):
		return self.UNARY_OP[op]

	@_write_vm
	def call(self, name, num_args):
		return "call {0} {1}".format(name, num_args)

	@_write_vm
	def function(self, name, num_local_vars):
		return "function {0} {1}".format(name, num_local_vars)

	@_write_vm
	def return_(self):
		return "return"

	def set_marker(self):
		self._markers.append(len(self._vm_code_blocks))

	def goto_marker(self):
		self._goto_marker = True

	def get_code(self):
		return "\n".join(self._vm_code_blocks)
