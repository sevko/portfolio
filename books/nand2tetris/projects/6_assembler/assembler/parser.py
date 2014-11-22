import re

SYMBOL_REGEX = "[{0}][{0}0-9]*".format("a-zA-Z_.$:")

def clean(string):
	remove_regex = ["[ \t]", "//.*$"]
	for regex in remove_regex:
		string = re.sub(regex, "", string)
	return string

def parse_label(string):
	match = re.search("^\({0}\)$".format(SYMBOL_REGEX), string)
	if match:
		return match.group().strip("()")
