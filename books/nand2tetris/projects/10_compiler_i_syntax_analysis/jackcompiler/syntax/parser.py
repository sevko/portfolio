GRAMMAR = {
	"class": (
		("KEYWORD", "class"),
		"class name",
		("SYMBOL", "{"),
		"class var dec",
		# "subroutine dec*",
		("SYMBOL", "}")
	),
	"class name": "IDENTIFIER",
	"class var dec": (
		[("KEYWORD", "static"), ("KEYWORD", "field")],
		"type",
		"var name",
		# "var names*",
		("SYMBOL", ";")
	),
	"type": (
		[
			("KEYWORD", "int"), ("KEYWORD", "char"), ("KEYWORD", "boolean"),
			("IDENTIFIER",)
		],
	),
	"var name": "IDENTIFIER",
	"var names": (
		("SYMBOL", ","),
		"var name"
	)
}

def parse(tokens):
	out = parse_syntax("class", tokens)
	print(to_string(out))
	return out

import sys
def parse_syntax(syntax_name, tokens, optional=False):
	syntax = GRAMMAR[syntax_name]

	if isinstance(syntax, str):
		if tokens[0][0] == syntax:
			return tokens.pop(0)
		else:
			# print(syntax, tokens[0])
			sys.exit(4)

	structure = []
	for item in syntax:
		# print(syntax_name, "-", tokens[0])
		if isinstance(item, list):
			for acceptable_match in item:
				if tokens[0] == acceptable_match:
					structure.append(tokens.pop(0))
					break
			else:
				# print(item, tokens)
				sys.exit(1)

		elif isinstance(item, tuple):
			if len(item) == 2:
				if tokens[0] == item:
					structure.append(tokens.pop(0))
				else:
					if not optional:
						# print(syntax_name, item, tokens)
						sys.exit(2)
			else:
				if tokens[0][0] == item[0]:
					structure.append(tokens.pop(0))
				else:
					sys.exit(5)

		else:
			if item.endswith("*"):
				item = item[:-1]
				matches = []
				match = parse_syntax(item, tokens)
				while match is not None:
					matches.append(match)
					match = parse_syntax(item, tokens, optional=True)
				structure.append((item, matches))
			else:
				structure.append(parse_syntax(item, tokens))

	return (syntax_name, structure)
