"""
Contains functions that tokenize raw Jack code.
"""

import re

# Maps token names to regular expression, for use by `tokenize()`.
TOKENS = [
	("KEYWORD", re.compile(r"({0})\b".format("|".join([
		"class", "constructor", "function", "method", "field", "static", "var",
		"int", "char", "boolean", "void", "true", "false", "null", "this",
		"let", "do", "if", "else", "while", "return"
	])))),
	("COMMENT", re.compile(r"//.*?$|/\*.*?\*/", re.DOTALL | re.MULTILINE)),
	("SYMBOL", re.compile(r"[{}\[\]().,;+*/&|<>=~-]")),
	("INTEGER", re.compile(r"[1-9]\d*(?=\W)")),
	("STRING", re.compile('"[^"\n]*"')),
	("IDENTIFIER", re.compile(r"[a-zA-Z_]\w*")),
	("WHITESPACE", re.compile(r"\s+"))
]

class TokenizerException(Exception):
	pass

def tokenize(code_string):
	"""
	Tokenizes an input string using `TOKENS` regular expressions.

	Args:
		code_string (string): Raw, unprocessed Jack source code. May contain
		comments and superfluous whitespace.

	Returns:
		(list of tuples) Each tuple represents a token, and contains two
		strings: the name of the token type (see `TOKENS`), and the token
		itself.

	Raises:
		TokenizerException: If an invalid (unrecognized) token was encountered
		during tokenization.
	"""

	tokens = []
	while code_string:
		for tok_type, regex in TOKENS:
			match = regex.match(code_string)
			if match:
				code_string = code_string[match.end():]
				if tok_type not in ("WHITESPACE", "COMMENT"):
					tokens.append((tok_type, match.group(0)))
				break
		else:
			err_msg = "No token match for these characters: `{0}`".format(
				re.match("[^\n]*", code_string).group(0)
			)
			raise TokenizerException(err_msg)
	return tokens
