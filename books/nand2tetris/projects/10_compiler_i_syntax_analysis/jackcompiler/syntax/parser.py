"""
Functions for parsing token streams according to the Jack grammar.
"""

import collections
from jackcompiler.syntax import tokenizer

class ParseTreeNode(object):
	"""
	A represenation of nodes in the parse tree assembled by the `GrammarRule`s
	in `GRAMMAR`.

	Attributes:
		name (string): The name assigned to this node, which collectively
			describes all of its children nodes. For instance, "arguments",
			"parameters", etc.
		children (list of (ParseTreeNode or tokenizer.Token)): A list of the
			`ParseTreeNode`s that belong to the grammar rule that assembled
			this `ParseTreeNode`.
	"""

	def __init__(self, name, children):
		self.name = name
		self.children = children

	def to_xml(self):
		"""
		Returns:
			A recursive XML representation of this ParseTreeNode, with tab
			indentation.
		"""

		body = "\n".join([
			"\t" + row
			for item in self.children
			for row in item.to_xml().split("\n")
		])
		return "<{0}>\n{1}\n</{0}>".format(self.name, body)

class ParserException(Exception):
	pass

def _add_rule(function):
	"""
	Decorator for use in defining `GrammarRule` methods. Appends the functions
	that they return to the instance's `self._rules` array (see
	`GrammarRule._rules`), and returns `self` to the caller to allow chained
	calls.
	"""

	def rule(*args):
		args[0]._rules.append(function(*args))
		return args[0]

	return rule

class GrammarRule(object):
	"""
	A representation of a grammatical rule, which supports:

		- matching exact tokens
		- matching token types
		- repeating rules
		- optional rules
		- etc. See method docstrings for more.

	Attributes:
		name (string): The name assigned to this rule; lends itself to more
			useful ParserException messages in stack traces, and is updated ON
			THE FLY based on the name of the calling `GrammarRule` and this
			rule's `self.original_name` (see next).
		original_name (string): The original name assigned to this rule on
			`__init__()`. Used to reinitialize `self.name` after it was
			modified by a specific caller.
		_rules (list of functions): The list of functions queued up in this
			grammar rule, to be executed by `self()`. Added automatically
			by the `_add_rule()` decorator.
		_tokens (list of tokens): The list of tokens this grammar rule is
			assigned to operate on. Only needs to be specified when
			`self()` is called.
		_optional (bool): Whether or not this rule is optional. Indicates
			whether or not `ParserException` should be thrown on incompatible
			tokens in `self._tokens`.
	"""

	def __init__(self, name=""):
		self.original_name = name
		self.name = name
		self._rules = []

	@_add_rule
	def token(self, token_content):
		def get_token():
			if self._tokens[0].content == token_content:
				return self._tokens.pop(0)
			elif not self._optional:
				self.error(
					"Expecting token `{0}` but got `{1}`.",
					token_content, self._tokens.pop(0)
				)

		return get_token

	@_add_rule
	def token_type(self, token_type):
		def get_token_type():
			if self._tokens[0].type_ == token_type:
				return self._tokens.pop(0)
			elif not self._optional:
				self.error(
					"Expecting token type `{0}` but got `{1}`.",
					token_type, self._tokens.pop(0)
				)

		return get_token_type

	@_add_rule
	def once(self, rule_name):
		def get_once():
			rule = GRAMMAR[rule_name]
			rule.name = self.name + "." + rule.original_name
			match = rule(self._tokens, optional=self._optional)
			if match:
				return match
			elif not self._optional:
				self.error(
					"Expecting rule {0} but got {1}.",
					rule, self._tokens.pop(0)
				)

		return get_once

	@_add_rule
	def repeat(self, rule):
		def get_repeat():
			matches = []
			rule.name = self.name + "." + rule.name
			match = rule(self._tokens, optional=True)
			while match:
				matches.append(match)
				match = rule(self._tokens, optional=True)
			return matches

		return get_repeat

	@_add_rule
	def either(self, rules):
		def get_either():
			for rule in rules:
				rule.name = self.name + "." + rule.name
				match = rule(self._tokens, optional=True)
				if match:
					return match
			else:
				if not self._optional:
					self.error(
						"Expecting any of:\n{0}\nbut got {1}.",
						"\n".join(["\t" + str(rule) for rule in rules]),
						self._tokens.pop(0)
					)

		return get_either

	@_add_rule
	def optional(self, rule):
		def get_optional():
			rule.name = self.name + "." + rule.name
			match = rule(self._tokens, optional=True)
			if match:
				return match

		return get_optional

	def error(self, msg, *format_args):
		except_msg = "\nRule `{0}`. Error: {1}\n\nToken context:\n\t`{2}`"
		except_msg = except_msg.format(
			self.name,
			(msg.format(*format_args) if format_args else msg),
			", ".join([str(tok) for tok in self._tokens[:4]])
		)
		raise ParserException(except_msg)

	def __call__(self, tokens, optional=False):
		self._tokens = tokens
		self._optional = optional

		matches = []
		for rule in self._rules:
			sub_matches = rule()
			if sub_matches:
				if isinstance(sub_matches, list):
					matches.extend(sub_matches)
				else:
					matches.append(sub_matches)
				self._optional = False
			elif self._optional:
				return

		is_terminal = len(matches) == 1 and isinstance(
			matches[0], tokenizer.Token
		)
		return matches[0] if is_terminal else ParseTreeNode(self.name, matches)

	def __str__(self):
		return self.name

GRAMMAR = {
	"class": GrammarRule("class")
		.token("class")
		.once("class name")
		.token("{")
		.repeat(GrammarRule().once("class var dec"))
		.token("}"),

	"class name": GrammarRule("class name").token_type("IDENTIFIER"),
	"class var dec": GrammarRule("class var dec")
		.either([
			GrammarRule("static").token("static"),
			GrammarRule("field").token("field")
		])
		.once("type")
		.once("var name")
		.repeat(GrammarRule("args").token(",").once("var name"))
		.token(";"),
	"type": GrammarRule("type")
		.either([
			GrammarRule("int").token("int"),
			GrammarRule("char").token("char"),
			GrammarRule("boolean").token("boolean"),
			GrammarRule("IDENTIFIER").token_type("IDENTIFIER")
		]),
	"var name": GrammarRule("var name").token_type("IDENTIFIER")
}

def parse(tokens):
	return GRAMMAR["class"](tokens).to_xml()
