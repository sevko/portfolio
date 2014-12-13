"""
Contains functions that tokenize raw Jack code.
"""

import collections
import re
from xml.sax import saxutils

# Maps token names to regular expression, for use by `tokenize()`.
TOKENS = [
	("keyword", re.compile(r"({0})\b".format("|".join([
		"class", "constructor", "function", "method", "field", "static", "var",
		"int", "char", "boolean", "void", "true", "false", "null", "this",
		"let", "do", "if", "else", "while", "return"
	])))),
	("comment", re.compile(r"//.*?$|/\*.*?\*/", re.DOTALL | re.MULTILINE)),
	("symbol", re.compile(r"[{}\[\]().,;+*/&|<>=~-]")),
	("integerConstant", re.compile(r"\d+\b")),
	("stringConstant", re.compile('"[^"\n]*"')),
	("identifier", re.compile(r"[a-zA-Z_]\w*")),
	("whitespace", re.compile(r"\s+"))
]

class Token(object):
	"""
	Represents a textual token, or atomic unit of parsing, as assembled using
	the regular expressions in `TOKENS`.

	Attributes:
		type_ (string): The name of the token type; see `TOKENS`.
		content (string): The contents of the token, as matched by the regular
			expressions in `TOKENS`.
	"""

	def __init__(self, type_, content):
		self.type_ = type_
		self.content = content

	def to_xml(self):
		"""
		Returns:
			An XML representation of this Token.
		"""

		return "<{0}> {1} </{0}>".format(
			self.type_, saxutils.escape(self.content)
		)

	def __str__(self):
		return "({0}, `{1}`)".format(self.type_, self.content)

	def __eq__(self, other):
		return self.type_ == other.type_ and self.content == other.content

	def __ne__(self, other):
		return self.type_ != other.type_ or self.content != other.content

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
		token = get_token(code_string)
		if token is not None:
			code_string = code_string[len(token.content):]
			if token.type_ not in ("whitespace", "comment"):
				if token.type_ == "stringConstant":
					token.content = token.content.strip('"')
				tokens.append(token)
		else:
			err_msg = "No token match for these characters: `{0}`".format(
				re.match("[^\n]*", code_string).group(0)
			)
			raise TokenizerException(err_msg)
	return tokens

def get_token(code_string):
	"""
	Attempt to extract a token from the beginning of a raw source string.

	Args:
		code_string (code_string): Raw Jack source code.

	Returns:
		(string, string or None) If a match was found, return
		(token_type, token_string), where `token_type` is the name of a token
		type (see `Tokens`) and `token_string` is the content of the token. If
		none were, return None.
	"""

	for tok_type, regex in TOKENS:
		match = regex.match(code_string)
		if match:
			return Token(tok_type, match.group(0))
