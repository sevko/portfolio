class ParserException(Exception):
	pass

def add_rule(function):
	def rule(*args):
		args[0]._rules.append(function(*args))
		return args[0]

	return rule

class GrammarRule(object):
	def __init__(self, name):
		self.name = name
		self._rules = []

	@add_rule
	def token(self, token):
		def get_token():
			if self._tokens[0] == token:
				return [self._tokens.pop(0)]
			elif not self._optional:
				except_msg = "Expecting token {0} but got {1}.".format(
					token, self._tokens.pop(0)
				)
				raise ParserException(except_msg)

		return get_token

	@add_rule
	def token_type(self, token_type):
		def get_token_type():
			if self._tokens[0][0] == token_type:
				return [self._tokens.pop(0)]
			elif not self._optional:
				except_msg = "Expecting token type {0} but got {1}.".format(
					token_type, self._tokens.pop(0)
				)
				raise ParserException(except_msg)

		return get_token_type

	@add_rule
	def once(self, rule):
		def get_once():
			match = GRAMMAR[rule].parse(self._tokens, optional=self._optional)
			if match:
				return match
			elif not self._optional:
				except_msg = "Expecting rule {0} but got {1}.".format(
					rule, self._tokens.pop(0)
				)
				raise ParserException(except_msg)

		return get_once

	@add_rule
	def repeat(self, rule):
		def get_repeat():
			matches = []
			match = rule.parse(self._tokens, optional=True)
			while match:
				matches.extend(match)
				match = rule.parse(self._tokens, optional=True)
			return matches

		return get_repeat

	@add_rule
	def either(self, rules):
		def get_either():
			for rule in rules:
				match = rule.parse(self._tokens, optional=True)
				if match:
					return match
			else:
				if not self._optional:
					except_msg = \
						"Expecting any of:\n\n{0}\n\nbut got {1}.".format(
							"\n".join(["\t" + str(rule) for rule in rules]),
							self._tokens.pop(0)
						)
					raise ParserException(except_msg)

		return get_either

	@add_rule
	def optional(self, rule):
		def get_optional():
			match = rule.parse(self._tokens, optional=True)
			if match:
				return match

		return get_optional

	def parse(self, tokens, optional=False):
		self._tokens = tokens
		self._optional = optional

		matches = []
		for rule in self._rules:
			sub_matches = rule()
			if sub_matches:
				matches.extend(sub_matches)
			else:
				break

		return matches

	def __str__(self):
		return self.name

GRAMMAR = {
	"class": GrammarRule("class")
		.token(("KEYWORD", "class"))
		.once("class name")
		.token(("SYMBOL", "{"))
		.repeat(GrammarRule("class var dec").once("class var dec"))
		.token(("SYMBOL", "}")),

	"class name": GrammarRule("class name").token_type("IDENTIFIER"),
	"class var dec": GrammarRule("class var dec")
		.either([
			GrammarRule("static").token(("KEYWORD", "static")),
			GrammarRule("field").token(("KEYWORD", "field"))
		])
		.once("type")
		.once("var name")
		.token(("SYMBOL", ";")),
	"type": GrammarRule("type")
		.either([
			GrammarRule("int").token(("KEYWORD", "int")),
			GrammarRule("char").token(("KEYWORD", "char")),
			GrammarRule("boolean").token(("KEYWORD", "boolean")),
			GrammarRule("IDENTIFIER").token_type("IDENTIFIER")
		]),
	"var name": GrammarRule("var name").token_type("IDENTIFIER"),
	"var names": GrammarRule("var names")
		.token(("SYMBOL", ","))
		.once("var name")
}

def parse(tokens):
	return GRAMMAR["class"].parse(tokens)

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
