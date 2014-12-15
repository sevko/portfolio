"""
Contains a class that writes raw Hack Virtual Machine code, abstracting away
the language with several functions.
"""

class VMWriter(object):

	def __init__(self):
		self._vm_code_blocks = []

	def get_code(self):
		return "\n".join(self._vm_code_blocks)
