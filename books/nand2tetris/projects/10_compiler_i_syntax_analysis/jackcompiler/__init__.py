"""
A compiler that translates the Jack programming language to Hack virtual
machine instructions.
"""

from jackcompiler import syntax
from jackcompiler import compiler

def compile(code_string):
	"""
	Args:
		code_string (string): Raw Jack source code.

	Return:
		(string) The Hack virtual machine instructions compiled from
		`code_string`.
	"""

	return compiler.compile_ast(syntax.parse(code_string))
