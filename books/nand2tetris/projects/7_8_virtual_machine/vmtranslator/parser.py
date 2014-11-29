import re

from vmtranslator import operations

def clean_code(code):
	no_whitespace = re.sub(r"(\s)\1+", r"\1", code.strip("\n\r \t"))
	no_comments = re.sub("//.*", "", no_whitespace)
	return no_comments

def parse_line(line):
	for op_class in operations.operation_classes:
		op = op_class.from_string(line)
		if op is not None:
			return op
