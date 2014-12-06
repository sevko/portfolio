"""
Unit tests for the `jackcompiler.syntax` subpackage.
"""

import unittest

from jackcompiler.syntax import tokenizer

class TestTokenizer(unittest.TestCase):
	"""Test `jackcompiler.syntax.tokenizer`.
	"""

	def test_keyword_token(self):
		"""Test `KEYWORD` token detection.
		"""

		matches = ["class", "constructor/aaaa", "null  ", "while"]
		non_matches = ["classa", "random_word"]
		self._tokenize_test_cases(matches, non_matches, "KEYWORD")

	def test_comment_token(self):
		"""Test `COMMENT` token detection.
		"""

		matches = ["// 'tis a comment", "/*a\nmulti\nline\ncomment*/"]
		non_matches = ["not // a comment"]
		self._tokenize_test_cases(matches, non_matches, "COMMENT")

	def test_symbol_token(self):
		"""Test `SYMBOL` token detection.
		"""

		matches = ["{", "<", "=", "*"]
		non_matches = ["a{", " {", "p"]
		self._tokenize_test_cases(matches, non_matches, "SYMBOL")

	def test_integer_token(self):
		"""Test `INTEGER` token detection.
		"""

		matches = ["1234567", "1212  ", "2121 word"]
		non_matches = ["012121", "a12211"]
		self._tokenize_test_cases(matches, non_matches, "INTEGER")

	def test_string_token(self):
		"""Test `STRING` token detection.
		"""

		matches = ['"is a string"', '"string" non-string']
		non_matches = ['"\n"', "'string'", '"abcde']
		self._tokenize_test_cases(matches, non_matches, "STRING")

	def test_identifier_token(self):
		"""Test `IDENTIFIER` token detection.
		"""

		matches = ["valid", "v4l1d", "va_lid 12020", "class_", "resource_"]
		non_matches = ["0identifier", ">s", "-s", "class"]
		self._tokenize_test_cases(matches, non_matches, "IDENTIFIER")

	def test_whitespace_token(self):
		"""Test `WHITESPACE` token detection.
		"""

		matches = ["   ", "\t\n\r  \t ", " text"]
		non_matches = ["_", "_ "]
		self._tokenize_test_cases(matches, non_matches, "WHITESPACE")

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
			("KEYWORD", "class"),
			("IDENTIFIER", "Number"),
			("SYMBOL", "{"),
			("KEYWORD", "field"),
			("KEYWORD", "int"),
			("IDENTIFIER", "_value"),
			("SYMBOL", ";"),
			("KEYWORD", "constructor"),
			("IDENTIFIER", "Number"),
			("IDENTIFIER", "new"),
			("SYMBOL", "("),
			("KEYWORD", "int"),
			("IDENTIFIER", "value"),
			("SYMBOL", ")"),
			("SYMBOL", "{"),
			("KEYWORD", "let"),
			("IDENTIFIER", "_value"),
			("SYMBOL", "="),
			("IDENTIFIER", "value"),
			("SYMBOL", ";"),
			("SYMBOL", "}"),
			("SYMBOL", "}")
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
			self.assertIsInstance(token, tuple)
			self.assertEqual(token[0], token_type)

		for non_match in non_matches:
			token = tokenizer.get_token(non_match)
			if token is not None:
				self.assertIsInstance(token, tuple)
				self.assertNotEqual(token[0], token_type)
