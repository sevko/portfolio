"""
A simple recursive-descent JSON parser, implemented according to the JSON spec
at http://json.org/.
"""

import sys

class JsonParserException(Exception):
	"""
	The exception thrown by `JsonParser`'s methods.
	"""

	pass

class JsonParser(object):
	"""
	A JSON parser, bundled into a class because it needs to maintain some
	internal state (like current index in the input string, column/line number,
	etc.). To top-level parsing method is `parse_value()`, so sample usage
	might look like:

	>>> JsonParser("my json string").parse_value()
	"""

	def __init__(self, string):
		self.string = string
		self.string_ind = 0
		self.col_num = 1
		self.ln_num = 1

	def parse_value(self):
		"""
		The top-level parser.
		"""

		peeked_char = self.peek()

		if peeked_char == '"':
			return self.parse_string()

		elif peeked_char == "-" or peeked_char.isdigit():
			return self.parse_number()

		elif peeked_char == "{":
			return self.parse_object()

		elif peeked_char == "[":
			return self.parse_array()

		elif peeked_char in ["f", "t"]:
			return self.parse_boolean()

		elif peeked_char == "n":
			return self.parse_null()

		self.error("Failed to parse value.")

	def parse_object(self):
		"""
		Parse any number of space-delimited key-value pairs, enclosed in
		curly braces. Return the pairs in a dictionary.
		"""

		self.get("{")
		if self.peek() == "}":
			self.get("}")
			return {}

		fields = [self.parse_field()]
		while self.peek() == ",":
			self.get(",")
			fields.append(self.parse_field())

		self.get("}")
		return dict(fields)

	def parse_field(self):
		"""
		Parse a colon-separated key-value pair, returning `(key, value)`.
		"""

		key = self.parse_string()
		self.get(":")
		value = self.parse_value()
		return key, value

	def parse_array(self):
		"""
		Parse any number of comma-separated values enclosed in square brackets.
		Return a list of the values.
		"""

		self.get("[")
		if self.peek() == "]":
			self.get("]")
			return []

		values = [self.parse_value()]
		while self.peek() == ",":
			self.get(",")
			values.append(self.parse_value())

		self.get("]")
		return values

	def parse_boolean(self):
		return self.get("false" if self.peek() == "f" else "true")

	def parse_null(self):
		return self.get("null")

	def parse_number(self):
		"""
		Parse a number, which can optionally:
			* be negated
			* have a decimal component
			* have an exponent component (optionally negated)
		"""

		negate = self.peek() == "-"
		if negate:
			self.get("-")

		base_digits = self.parse_digits()

		if self.peek() == ".":
			self.get(".")
			decimal_digits = self.parse_digits()
			num = float(base_digits + "." + decimal_digits)
		else:
			num = int(base_digits)

		if self.peek().lower() == "e":
			self.get()
			peeked_char = self.peek()

			negate_power = peeked_char == "-"
			if peeked_char in ["+", "-"]:
				self.get()

			power = int(self.parse_digits())
			num *= 10 ** (-power if negate_power else power)

		return -num if negate else num

	def parse_digits(self):
		"""
		Parse one or more digits.
		"""

		peeked_char = self.peek()
		if not peeked_char.isdigit():
			self.error("Expecting 0-9, but got: `{}`".format(peeked_char))

		digits = [self.get()]
		while self.peek().isdigit():
			digits.append(self.get())

		return "".join(digits)

	def parse_string(self):
		"""
		Parse a string enclosed in double quotes, handling escaped and
		hex-encoded unicode characters. Return the string.
		"""

		escapable_chars = {
			'"': '"',
			"\\": "\\",
			"/": "/",
			"b": "\b",
			"f": "\f",
			"n": "\n",
			"r": "\r",
			"t": "\t"
		}

		self.get('"')
		chars = []
		while self.peek(skip_whitespace=False) != '"':
			char = self.get(skip_whitespace=False)
			if char == "\\":
				escaped_char = self.get()
				if escaped_char in escapable_chars:
					chars.append(escapable_chars[escaped_char])
				elif escaped_char == "u":
					hex_digits = "".join(self.get() for _ in range(4))

					try:
						unicode_val = int(hex_digits, 16)
					except ValueError:
						self.error(
							"Expecting 4 hexadecimal digits after `\\u`, but "
							"got: `{}`".format(hex_digits))

					chars.append(chr(unicode_val))
				else:
					self.error(
						"`{}` is not an escapable char! The escapable "
						"chars are: {}, or a `u` followed by 4 hex digits "
						"(to represent a unicode character)".format(
							escaped_char, ", ".join(escapable_chars)))
			else:
				chars.append(char)

		self.get('"')
		return "".join(chars)

	def get(self, expected_str=None, skip_whitespace=True):
		"""
		Advance the parser along the input string, skipping any preceding
		whitespace if `skip_whitespace` is True. If `expected_str` is not None,
		a string of the same length will be read from the input string and
		compared against it for equality; if they don't match, a
		`JsonParserException` will be raised. Otherwise, read just 1 character.
		On successful completion, the read character(s) will be returned.
		"""

		if skip_whitespace:
			self.skip_whitespace()

		if expected_str is None:
			curr_char = self.string[self.string_ind]
			self.string_ind += 1
			self.col_num += 1
			return curr_char
		else:
			curr_str = self.string[
				self.string_ind:self.string_ind + len(expected_str)]
			if curr_str == expected_str:
				self.string_ind += len(expected_str)
				self.col_num += len(expected_str)
				return expected_str
			else:
				self.error(
					"Expecting `{}`, but got `{}`.".format(
						expected_str, curr_str))

	def peek(self, skip_whitespace=True):
		"""
		Peek at the next character in the input string without actually
		advancing the parser. Skips any whitespace if `skip_whitespace` is
		True.
		"""

		if skip_whitespace:
			self.skip_whitespace()
		return self.string[self.string_ind]

	def skip_whitespace(self):
		"""
		Advance the parser past any immediate whitespace in the input string.
		"""

		while True:
			curr_char = self.string[self.string_ind]
			if curr_char not in [" ", "\n", "\t"]:
				break

			if curr_char == "\n":
				self.ln_num += 1
				self.col_num = 1

			elif curr_char == "\t":
				self.col_num += 8 - (self.col_num - 1) % 8

			else:
				self.col_num += 1

			self.string_ind += 1

	def error(self, err_msg):
		"""
		Raise a `JsonParserException` with the `err_msg` message, adding a
		bunch of useful context to it (like the current line number, a visual
		illustration of where the error occured in the input string, etc.).
		"""

		num_context_lines = 5
		start_ln_ind = 0 if self.ln_num < num_context_lines \
			else self.ln_num - 5
		input_context = self.string.split("\n")[start_ln_ind:self.ln_num]
		indent_str = "\t"
		indented = "\n".join([indent_str + ln for ln in input_context])
		error_pointer = indent_str + " " * (self.col_num - 1) + "^"

		raise JsonParserException(
			"Error on line {}, column {}:\n{}\n{}\n{}".format(
				self.ln_num, self.col_num, indented, error_pointer, err_msg))

if __name__ == "__main__":
	if len(sys.argv) >= 2:
		with open(sys.argv[0]) as json_fp:
			input_str = json_fp.read()
	else:
		input_str = sys.stdin.read()

	print(JsonParser(input_str).parse_value())
