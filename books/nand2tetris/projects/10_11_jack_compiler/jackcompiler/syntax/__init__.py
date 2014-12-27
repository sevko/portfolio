"""
The syntax analysis sub-package of the Jack compiler, which contains modules
for tokenizing/parsing Jack source code.
"""

from jackcompiler.syntax import tokenizer
from jackcompiler.syntax import parser

def parse(code_string):
	"""
	Args:
		code_string (string): Raw Jack source code.

	Returns:
		(jackcompiler.syntax.AbstractSyntaxTree) The Abstract Syntax Tree
		generated from `code_string`.
	"""

	return parser.parse(tokenizer.tokenize(code_string))
