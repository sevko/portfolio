"""
Miscellaneous parsing utility functions.
"""

import re

from vmtranslator import operations

def clean_code(code):
	"""
	Normalizes repeated whitespace and removes comments using regular
	expressions.

	Args:
		code (string): Raw, stringified Virtual Machine code.

	Returns:
		(string) `code`, with any repeated same whitespace characters (eg two
			tabs in a row) reduced to one such characters, and `//` comments
			removed. Also strips leading and trailing whitespace.
	"""

	no_whitespace = re.sub(r"(\s)\1+", r"\1", code.strip("\n\r \t"))
	no_comments = re.sub("//.*", "", no_whitespace)
	return no_comments

def parse_line(line):
	"""
	Args:
		line (string): A line of Virtual Machine code.

	Returns:
		(None or Operation) If an operation was successfully parsed out using
			the `from_string()` methods implemented by classes in the
			`operations` sub-package, return it; otherwise, None.
	"""

	for op_class in operations.operation_classes:
		op = op_class.from_string(line)
		if op is not None:
			return op
