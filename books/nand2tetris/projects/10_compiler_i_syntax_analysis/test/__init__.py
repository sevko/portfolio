"""
Entry point for the jackcompiler unit tests.
"""

import unittest
import glob

def run_tests():
	"""
	Execute all `test_*.py` files in the top-level of the `test` package (in
	ohter words, this directory) via `unittest.main()`.
	"""

	for path in glob.glob("test/test_*.py"):
		module = path[:-3].replace("/", ".")
		unittest.main(module, verbosity=2)
