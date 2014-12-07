GRAMMAR = {
	"class": (
		("token", ("KEYWORD", "class")),
		("once", "class name"),
		("token", ("SYMBOL", "{")),
		("repeat", ("once", "class var dec")),
		# ("repeat", ("once", "subroutine dec")),
		("token", ("SYMBOL", "}"))
	),
	"class name": (("token type", "IDENTIFIER"),),
	"class var dec": (
		("either", [
			("token", ("KEYWORD", "static")),
			("token", ("KEYWORD", "field"))
		]),
		("once", "type"),
		("once", "var name"),
		# ("repeat", ("once", "var names")),
		("token", ("SYMBOL", ";"))
	),
	"type": (("either", [
		("token", ("KEYWORD", "int")),
		("token", ("KEYWORD", "char")),
		("token", ("KEYWORD", "boolean")),
		("token type", ("IDENTIFIER",))
	]),),
	"var name": (("token type", "IDENTIFIER"),),
	"var names": (
		("token", ("SYMBOL", ",")),
		("once", "var name")
	)
}

def parse(tokens):
	out = parse_rule(GRAMMAR["class"], tokens)
	# print(to_string(out))
	return out

class ParserException(Exception):
	pass

import sys
def parse_rule(grammar_rule, tokens, optional=False):
	grammar_object = []

	for component in grammar_rule:
		print(str(tokens[0]).ljust(30), component)
		if component[0] == "token":
			if tokens[0] == component[1]:
				grammar_object.append(tokens.pop(0))
			elif not optional:
				except_msg = "Expecting token `{0}` but got `{1}`.".format(
					component[1], tokens.pop(0)
				)
				raise ParserException(except_msg)
			else:
				break

		elif component[0] == "token type":
			if tokens[0][0] == component[1]:
				grammar_object.append(tokens.pop(0))
			elif not optional:
				except_msg = "Expecting token type `{0}` but got `{1}`".format(
					component[1], tokens.pop(0)
				)
				raise ParserException(except_msg)
			else:
				break

		elif component[0] == "token type":
			if tokens[0][0] == component[1]:
				grammar_object.append(tokens.pop(0))
			elif not optional:
				except_msg = "Expecting token type `{0}` but got `{1}`".format(
					component[1], tokens.pop(0)
				)
				raise ParserException(except_msg)
			else:
				break

		elif component[0] == "token type":
			if tokens[0][0] == component[1]:
				grammar_object.append(tokens.pop(0))
			elif not optional:
				except_msg = "Expecting token type `{0}` but got `{1}`".format(
					component[1], tokens.pop(0)
				)
				raise ParserException(except_msg)
			else:
				break

		elif component[0] == "once":
			match = parse_rule(GRAMMAR[component[1]], tokens, optional=optional)
			if match:
				grammar_object.extend(match)
			elif not optional:
				except_msg = "Expecting `{0}` but got `{1}`.".format(
					GRAMMAR[component[1]], tokens[0]
				)
				raise ParserException(except_msg)
			else:
				break

		elif component[0] == "token type":
			if tokens[0][0] == component[1]:
				grammar_object.append(tokens.pop(0))
			elif not optional:
				except_msg = "Expecting token type `{0}` but got `{1}`".format(
					component[1], tokens.pop(0)
				)
				raise ParserException(except_msg)
			else:
				break

		elif component[0] == "repeat":
			match = parse_rule([component[1]], tokens, optional=True)
			while match:
				grammar_object.extend(match)
				match = parse_rule([component[1]], tokens, optional=True)

		elif component[0] == "either":
			for rule in component[1]:
				match = parse_rule([rule], tokens, optional=True)
				if match:
					grammar_object.extend(match)
					break
			else:
				if not optional:
					except_msg = "Expecting any of `{0}` but got `{1}`.".format(
						component[1], tokens[0]
					)
					raise ParserException(except_msg)
				else:
					break

		elif component[0] == "optional":
			match = parse_rule([component[1]], tokens, optional=True)
			if match:
				grammar_object.extend(match)

	return grammar_object

def to_string(tree):
	if isinstance(tree[1], str):
		body = "\t" + tree[1]
	else:
		body = [to_string(item) for item in tree[1]]
		body = "\n".join([
			"\t" + row
			for item in tree[1]
			for row in to_string(item).split("\n")
		])
	return "<{0}>\n{1}\n</{0}>".format(tree[0], body)
