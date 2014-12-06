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
