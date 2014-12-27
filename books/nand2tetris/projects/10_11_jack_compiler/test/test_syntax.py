"""
Unit tests for the `jackcompiler.syntax` subpackage.
"""

import unittest

from jackcompiler.syntax import tokenizer
from jackcompiler.syntax import parser

class TestTokenizer(unittest.TestCase):
	"""Test `jackcompiler.syntax.tokenizer`.
	"""

	def test_keyword_token(self):
		"""Test `keyword` token detection.
		"""

		matches = ["class", "constructor/aaaa", "null  ", "while"]
		non_matches = ["classa", "random_word"]
		self._tokenize_test_cases(matches, non_matches, "keyword")

	def test_comment_token(self):
		"""Test `comment` token detection.
		"""

		matches = ["// 'tis a comment", "/*a\nmulti\nline\ncomment*/"]
		non_matches = ["not // a comment"]
		self._tokenize_test_cases(matches, non_matches, "comment")

	def test_symbol_token(self):
		"""Test `symbol` token detection.
		"""

		matches = ["{", "<", "=", "*"]
		non_matches = ["a{", " {", "p"]
		self._tokenize_test_cases(matches, non_matches, "symbol")

	def test_integer_token(self):
		"""Test `integerConstant` token detection.
		"""

		matches = ["1234567", "1212  ", "2121 word"]
		non_matches = ["_012121", "a12211"]
		self._tokenize_test_cases(matches, non_matches, "integerConstant")

	def test_string_token(self):
		"""Test `stringConstant` token detection.
		"""

		matches = ['"is a string"', '"string" non-string']
		non_matches = ['"\n"', "'string'", '"abcde']
		self._tokenize_test_cases(matches, non_matches, "stringConstant")

	def test_identifier_token(self):
		"""Test `identifier` token detection.
		"""

		matches = ["valid", "v4l1d", "va_lid 12020", "class_", "resource_"]
		non_matches = ["0identifier", ">s", "-s", "class"]
		self._tokenize_test_cases(matches, non_matches, "identifier")

	def test_whitespace_token(self):
		"""Test `whitespace` token detection.
		"""

		matches = ["   ", "\t\n\r  \t ", " text"]
		non_matches = ["_", "_ "]
		self._tokenize_test_cases(matches, non_matches, "whitespace")

	def test_tokenize(self):
		"""Test whether `tokenize()` properly tokenizes a valid source string
		and raises a TokenizerException for an invalid one.
		"""

		src_string = """
			/**
			 Multiline comment.
			 */
			class Number {
				field int _value; // inline comment

				constructor Number new(int value){
					let _value = value;
				}
			}
			"""

		tokens = [
			tokenizer.Token("keyword", "class"),
			tokenizer.Token("identifier", "Number"),
			tokenizer.Token("symbol", "{"),
			tokenizer.Token("keyword", "field"),
			tokenizer.Token("keyword", "int"),
			tokenizer.Token("identifier", "_value"),
			tokenizer.Token("symbol", ";"),
			tokenizer.Token("keyword", "constructor"),
			tokenizer.Token("identifier", "Number"),
			tokenizer.Token("identifier", "new"),
			tokenizer.Token("symbol", "("),
			tokenizer.Token("keyword", "int"),
			tokenizer.Token("identifier", "value"),
			tokenizer.Token("symbol", ")"),
			tokenizer.Token("symbol", "{"),
			tokenizer.Token("keyword", "let"),
			tokenizer.Token("identifier", "_value"),
			tokenizer.Token("symbol", "="),
			tokenizer.Token("identifier", "value"),
			tokenizer.Token("symbol", ";"),
			tokenizer.Token("symbol", "}"),
			tokenizer.Token("symbol", "}")
		]

		self.assertEqual(tokenizer.tokenize(src_string), tokens)

		self.assertRaises(
			tokenizer.TokenizerException, tokenizer.tokenize, "0invalid"
		)

	def _tokenize_test_cases(self, matches, non_matches, token_type):
		"""
		Test matches/non-matches against a specified token-type.

		Args:
			matches (list of string): Text that matches the regex for
				`token_type`.
			non_matches (list of string): Text that does not match the regex
				for `token_type`.
			token_type (string): The name of the token type to match against
				(see `jackcompiler.syntax.tokenizer.TOKENS`).
		"""

		for match in matches:
			token = tokenizer.get_token(match)
			self.assertIsInstance(token, tokenizer.Token)
			self.assertEqual(token.type_, token_type)

		for non_match in non_matches:
			token = tokenizer.get_token(non_match)
			if token is not None:
				self.assertIsInstance(token, tokenizer.Token)
				self.assertNotEqual(token.type_, token_type)
