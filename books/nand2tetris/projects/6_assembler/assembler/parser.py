"""
Miscellaneous functions for parsing raw assembly.
"""

import re

SYMBOL_REGEX = "[{0}][{0}0-9]*".format("a-zA-Z_.$:")

class AssemblerError(Exception):
	pass

def clean(string):
	"""
	Args:
		string (string): A raw string, as encountered in unprocessed Assembly.

	Returns:
		`string` with all whitespace and comments stripped.
	"""

	remove_regex = ["[ \t]", "//.*$"]
	for regex in remove_regex:
		string = re.sub(regex, "", string)
	return string

def parse_label(string):
	"""
	Args:
		string (string): A raw assembly row (sans newlines), as cleaned with
			`clean()`, which potentially contains a label.

	Return:
		The contents of the label, if one was found; otherwise, `None`.
	"""

	match = re.search("^\({0}\)$".format(SYMBOL_REGEX), string)
	if match:
		return match.group().strip("()")
