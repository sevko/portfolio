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
		NON_TERMINAL (list of strings): The names of all non-terminal nodes,
			for which individual `ParseTreeNode`s should be returned by
			`GrammarRule.__call__()`.
	"""

	NON_TERMINAL = [
		"class", "classVarDec", "subroutineDec", "parameterList",
		"subroutineBody", "varDec", "statements", "whileStatement",
		"ifStatement", "returnStatement", "letStatement", "doStatement",
		"expression", "term", "expressionList"
	]

	def __init__(self, name, children):
		self.name = name
		self.children = children

	def to_xml(self):
		"""
		Returns:
			A recursive XML representation of this ParseTreeNode, with tab
			indentation.
		"""


		if self.children:
			children = "\n".join([
				"\t" + row
				for item in self.children
				for row in item.to_xml().split("\n")
			])
			body = "\n{0}\n".format(children)
		else:
			body = "\n"

		return "<{0}>{1}</{0}>".format(self.name, body)

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
			useful ParserException messages in stack traces.
		_rules (list of functions): The list of functions queued up in this
			grammar rule, to be executed by `self()`. Added automatically
			by the `_add_rule()` decorator.
		_tokens (list of tokens): The list of tokens this grammar rule is
			assigned to operate on. Only needs to be specified when
			`self()` is called.
		_optional (bool): Whether or not this rule is optional. Indicates
			whether or not `ParserException` should be thrown on incompatible
			tokens in `self._tokens`.
		_look_ahead (None or function): Optionally set by `self.look_ahead()`;
			if not None, it's used to perform a look-ahead verification of the
			token-stream to confirm that the remainder of the rule should be
			applied. Used to resolve ambiguity beyond the first token between
			two `GrammarRule`s, so that one of them isn't partly applied (and
			thus changes the token stream) before figuring out that it doesn't
			apply to the remaining tokens.
	"""

	def __init__(self, name=""):
		self.name = name
		self._rules = []
		self._look_ahead = None

	@_add_rule
	def token(self, token_content):
		"""
		Args:
			token_content (string): The exact token content to match (see
				`tokenizer.Token.content`).
		"""

		def get_token():
			if self._tokens[0].content == token_content:
				return self._is_look_ahead or self._tokens.pop(0)
			elif not self._optional:
				self.error(
					"Expecting token content `{0}` but got `{1}`",
					token_content, self._tokens[0]
				)

		return get_token

	@_add_rule
	def token_type(self, token_type):
		"""
		Args:
			token_type (string): The exact token type to match (see
				`tokenizer.Token.type_`).
		"""

		def get_token_type():
			if self._tokens[0].type_ == token_type:
				return self._is_look_ahead or self._tokens.pop(0)
			elif not self._optional:
				self.error(
					"Expecting token type `{0}` but got `{1}`",
					token_type, self._tokens[0]
				)

		return get_token_type

	@_add_rule
	def once(self, rule_name):
		"""
		Args:
			rule_name (string): The name of the rule in the global `GRAMMAR`
				dictionary (see its keys) to execute.
		"""

		def get_once():
			match = GRAMMAR[rule_name](self._tokens, optional=self._optional)
			if match:
				return match
			elif not self._optional:
				self.error(
					"Expecting match for rule `{0}` but got `{1}`.",
					rule_name, self._tokens[0]
				)

		return get_once

	@_add_rule
	def repeat(self, rule):
		"""
		Args:
			rule (`GrammarRule`): The `GrammarRule` to apply as many times as
				possible to `self._tokens`.
		"""

		def get_repeat():
			matches = []
			match = rule(self._tokens, optional=True)
			while match:
				if isinstance(match, list):
					matches.extend(match)
				else:
					matches.append(match)
				match = rule(self._tokens, optional=True)

			return matches

		return get_repeat

	@_add_rule
	def either(self, rules):
		"""
		Args:
			rules (list of `GrammarRule`): Any number of `GrammarRule`s to
			apply to `self._tokens`. Once a match is found, no remaining
			`GrammarRule`s are executed.
		"""

		def get_either():
			for rule in rules:
				match = rule(self._tokens, optional=True)
				if match:
					return match
			else:
				if not self._optional:
					self.error(
						"Expecting either one of {0} but got `{1}`",
						", ".join(["`{0}`".format(rule) for rule in rules]),
						self._tokens[0]
					)

		return get_either

	@_add_rule
	def optional(self, rule):
		"""
		Args:
			rule (`GrammarRule`): A rule to optionally apply to `self._tokens`.
				Won't through an exception on incompatible tokens.
		"""

		def get_optional():
			match = rule(self._tokens, optional=True)
			if match:
				return match
			elif rule.name in ParseTreeNode.NON_TERMINAL:
				return ParseTreeNode(rule.name, [])
			else:
				return []

		return get_optional

	def look_ahead(self, rule):
		"""
		Args:
			rule (`GrammarRule`): The rule to use as a look-ahead for this
			`GrammarRule`; if it doesn't validate `self._tokens`, no rules in
			`self._rules` will be executed.
		"""

		self._look_ahead = rule
		return self

	def error(self, msg, *format_args):
		"""
		Raise a `ParserException` with a message that's padded with useful
		debugging information. Preferable to direct `raise ParserException`
		statements.

		Args:
			msg (string): The message to emit as part of `ParserException`.
			*format_args (varargs): Any arguments to pass to `.format()` on
				`msg`.

		Raises:
			ParserException
		"""

		except_msg = "\nRule `{0}`. Error: {1}\n\nToken context:\n\t`{2}`"
		except_msg = except_msg.format(
			self.name,
			(msg.format(*format_args) if format_args else msg),
			", ".join([str(tok) for tok in self._tokens[:4]])
		)
		raise ParserException(except_msg)

	def __call__(self, tokens, optional=False, look_ahead=False):
		"""
		Args:
			tokens (list of `tokenizer.Token`): The token stream for this rule
				to operate on.
			optional (bool, optional): Indicates whether this rule is optional
				or not, which determines whether `ParserException`s are thrown
				on incompatible tokens in `tokens`.
			look_ahead (bool, optional): Indicates whether this rule is only a
				look-ahead, and thus whether it should or shouldn't modify
				`tokens`.

		Returns:
			(list or `ParseTreeNode`) A `ParseTreeNode` if this rule is an
				official non-terminal (its name must match
				`ParseTreeNode.NON_TERMINAL`); otherwise a list of all the
				`ParseTreeNode`/`tokenizer.Token`s matched by its sub-rules.
		"""

		self._tokens = tokens
		self._optional = optional or look_ahead
		self._is_look_ahead = look_ahead

		if self._look_ahead:
			if not self._look_ahead([tokens[1]], look_ahead=True):
				return

		matches = []
		for rule in self._rules:
			match = rule()
			if match is not None:
				if isinstance(match, list):
					matches.extend(match)
				else:
					matches.append(match)
				self._optional = False

			elif self._optional:
				return None

		# Detect pass-through rules, like:
		# `.repeat(GrammarRule("parameterList").once("parameterList"))`.
		if len(self._rules) == 1 and \
			len(matches) == 1 and \
			isinstance(matches[0], ParseTreeNode) and \
			self.name == matches[0].name:
			return matches[0]

		else:
			return ParseTreeNode(self.name, matches) if \
				self.name in ParseTreeNode.NON_TERMINAL else matches

	def __str__(self):
		return self.name

GRAMMAR = {
	"class": GrammarRule("class")
		.token("class")
		.once("className")
		.token("{")
		.repeat(GrammarRule().once("classVarDec"))
		.repeat(GrammarRule().once("subroutineDec"))
		.token("}"),
	"classVarDec": GrammarRule("classVarDec")
		.either([
			GrammarRule("static").token("static"),
			GrammarRule("field").token("field")
		])
		.once("type")
		.once("varName")
		.repeat(GrammarRule("args").token(",").once("varName"))
		.token(";"),
	"type": GrammarRule("type")
		.either([
			GrammarRule("int").token("int"),
			GrammarRule("char").token("char"),
			GrammarRule("boolean").token("boolean"),
			GrammarRule("identifier").token_type("identifier")
		]),
	"subroutineDec": GrammarRule("subroutineDec")
		.either([
			GrammarRule("type").token("constructor"),
			GrammarRule("type").token("function"),
			GrammarRule("type").token("method")
		])
		.either([
			GrammarRule("returnType").token("void"),
			GrammarRule("returnType").once("type"),
		])
		.token_type("identifier")
		.token("(")
		.optional(GrammarRule("parameterList").once("parameterList"))
		.token(")")
		.once("subroutineBody"),
	"subroutineBody": GrammarRule("subroutineBody")
		.token("{")
		.repeat(GrammarRule().once("varDec"))
		.once("statements")
		.token("}"),
	"varDec": GrammarRule("varDec")
		.token("var")
		.once("type")
		.once("varName")
		.repeat(GrammarRule().token(",").once("varName"))
		.token(";"),
	"statements": GrammarRule("statements")
		.repeat(GrammarRule().once("statement")),
	"statement": GrammarRule("statement").either([
		GrammarRule().once("letStatement"),
		GrammarRule().once("ifStatement"),
		GrammarRule().once("whileStatement"),
		GrammarRule().once("doStatement"),
		GrammarRule().once("returnStatement")
	]),
	"letStatement": GrammarRule("letStatement")
		.token("let")
		.token_type("identifier")
		.optional(
			GrammarRule("index").token("[").once("expression").token("]")
		)
		.token("=")
		.once("expression")
		.token(";"),
	"ifStatement": GrammarRule("ifStatement")
		.token("if")
		.token("(")
		.once("expression")
		.token(")")
		.token("{")
		.once("statements")
		.token("}")
		.optional(
			GrammarRule("elseBody")
				.token("else")
				.token("{")
				.once("statements")
				.token("}")
		),
	"whileStatement": GrammarRule("whileStatement")
		.token("while")
		.token("(")
		.once("expression")
		.token(")")
		.token("{")
		.once("statements")
		.token("}"),
	"doStatement": GrammarRule("doStatement")
		.token("do")
		.once("subroutineCall")
		.token(";"),
	"returnStatement": GrammarRule("returnStatement")
		.token("return")
		.optional(GrammarRule("returnValue").once("expression"))
		.token(";"),
	"expression": GrammarRule("expression")
		.once("term")
		.repeat(
			GrammarRule("terms")
				.once("op")
				.once("term")
		),
	"term": GrammarRule("term")
		.either([
			GrammarRule().token("(").once("expression").token(")"),
			GrammarRule().token_type("integerConstant"),
			GrammarRule().token_type("stringConstant"),
			GrammarRule().once("keywordConstant"),
			GrammarRule().once("unaryOp").once("term"),
			GrammarRule()
				.look_ahead(GrammarRule().token("["))
				.once("varName")
				.token("[")
				.once("expression")
				.token("]"),
			GrammarRule().look_ahead(
				GrammarRule().either([
					GrammarRule().token("("),
					GrammarRule().token(".")
				])
			).once("subroutineCall"),
			GrammarRule().once("varName")
		]),
	"subroutineCall": GrammarRule("subroutineCall")
		.either([
			GrammarRule()
				.look_ahead(GrammarRule().token("("))
				.once("subroutineName")
				.token("(")
				.once("expressionList")
				.token(")"),
			GrammarRule()
				.either([
					GrammarRule().once("className"),
					GrammarRule().once("varName")
				])
				.token(".")
				.once("subroutineName")
				.token("(")
				.once("expressionList")
				.token(")")
		]),
	"expressionList": GrammarRule("expressionList")
		.optional(
			GrammarRule("firstExpression")
				.once("expression")
				.repeat(GrammarRule("moreExpressions")
					.token(",")
					.once("expression")
				)
		),
	"op": GrammarRule("op")
		.either([
			GrammarRule().token("+"),
			GrammarRule().token("-"),
			GrammarRule().token("*"),
			GrammarRule().token("/"),
			GrammarRule().token("&"),
			GrammarRule().token("|"),
			GrammarRule().token("<"),
			GrammarRule().token(">"),
			GrammarRule().token("=")
		]),
	"unaryOp": GrammarRule("op")
		.either([
			GrammarRule().token("-"),
			GrammarRule().token("~")
		]),
	"keywordConstant": GrammarRule("keywordConstant")
		.either([
			GrammarRule().token("true"),
			GrammarRule().token("false"),
			GrammarRule().token("null"),
			GrammarRule().token("this"),
		]),
	"parameterList": GrammarRule("parameterList")
		.once("type")
		.token_type("identifier")
		.repeat(
			GrammarRule("moreParameters")
				.token(",")
				.once("type")
				.token_type("identifier")
		),
	"subroutineName": GrammarRule("subroutineName").token_type("identifier"),
	"className": GrammarRule("className").token_type("identifier"),
	"varName": GrammarRule("varName").token_type("identifier")
}

def parse(tokens):
	return GRAMMAR["class"](tokens)
