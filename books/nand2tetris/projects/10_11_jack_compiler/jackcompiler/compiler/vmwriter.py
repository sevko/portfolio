"""
Contains a class that writes raw Hack Virtual Machine code, abstracting away
the language with several functions.
"""

def _write_vm(function):
	"""
	Decorator wrapping all `VMWriter` methods responsible for writing VM
	instruction blocks to the writer's internal buffer.

	Args:
		function (func): The function received by the decorator.

	Returns:
		A revised function which takes the output of the original and adds
		it to the `VMWriter`'s internal `_vm_code_blocks`. Either appended or
		inserted to a certain index depending on `self._goto_marker`.
	"""

	def _write_vm_block(*args):
		block = function(*args)
		if block is not None:
			if args[0]._goto_marker:
				args[0]._vm_code_blocks.insert(args[0]._markers.pop(), block)
				args[0]._goto_marker = False
			else:
				args[0]._vm_code_blocks.append(block)

	return _write_vm_block

class VMWriter(object):
	"""
	A class that facilitates writing Virtual Machine code by maintaing an
	internal buffer of instruction blocks that's updated by various methods.

	Attributes:
		_vm_code_blocks (list of strings): A list of Virtual Machine
			instruction blocks written by the various `@_write_vm` methods.
		_markers (list of ints): "Breakpoints" set by `set_marker()` and
			visited by `goto_marker()`, which represent indexes in
			`_vm_code_blocks`. Used to facilitate writing instruction blocks to
			indexes that have already been passed over and contain other
			instructions, as is the case when a VM instruction depends on the
			Jack code that follows it and passing over the code twice isn't
			desirable.
		_goto_marker (bool): Whether or not the next instruction should be
			written to the index of `_vm_code_blocks` contained in the last
			item of _markers`.
		FUNC_OP (dict): Maps symbols for operations that are NOT part of the
		Jack VM spec to their respective function names.
		BINARY_OP (dict): Maps symbols for binary operations to their
			respective VM instruction names.
		UNARY_OP (dict): Maps symbols for unary operations to their
			respective VM instruction names.
	"""

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

	@_write_vm
	def label(self, label):
		return "label " + label

	@_write_vm
	def goto(self, label):
		return "goto " + label

	@_write_vm
	def if_goto(self, label):
		return "if-goto " + label

	def set_marker(self):
		"""
		Adds a marker that can be revisited with `goto_marker` to
		`self._markers`.
		"""

		self._markers.append(len(self._vm_code_blocks))

	def goto_marker(self):
		"""
		Indicates that the next instruction written with any of the
		`@_write_vm` methods should be written to the index of
		`_vm_code_blocks` contained in the last item of `_markers`.
		"""

		self._goto_marker = True

	def get_code(self):
		return "\n".join(self._vm_code_blocks)
