"""
Unit tests for the `jackcompiler.syntax` subpackage.
"""

import unittest

from jackcompiler.syntax import tokenizer

class TestTokenizer(unittest.TestCase):

	def setUp(self):
		pass

	def test_keyword(self):
		keywords = ["class", "constructor/aaaa", "null  ", "while"]
		non_keywords = ["classa", "random_word"]

		for keyword in keywords:
			token = tokenizer.get_token(keyword)
			self.assertIsInstance(token, tuple)
			self.assertEqual(token[0], "KEYWORD")

		for keyword in non_keywords:
			token = tokenizer.get_token(keyword)
			self.assertIsInstance(token, tuple)
			self.assertNotEqual(token[0], "KEYWORD")

	def _test_foo(self):
		print("""
		FOO
		FOO
		FOO""")
