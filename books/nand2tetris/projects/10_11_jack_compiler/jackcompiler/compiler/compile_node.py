from jackcompiler.syntax import parser
from jackcompiler.syntax import tokenizer

indent = 0 # hack: REMOVE AFTER COMPLETION

def compile_(node, vm_writer, sym_tables):
	global indent # hack: REMOVE AFTER COMPLETION
	# print("  " * indent + node.name) # hack: REMOVE AFTER COMPLETION

	compile_func_name = "_compile_" + node.name
	globals_dict = globals()
	indent += 1 # hack: REMOVE AFTER COMPLETION
	if compile_func_name in globals_dict:
		globals_dict[compile_func_name](node, vm_writer, sym_tables)
	else:
		for child in node.children:
			if isinstance(child, parser.ParseTreeNode):
				compile_(child, vm_writer, sym_tables)
	indent -= 1 # hack: REMOVE AFTER COMPLETION

def _compile_class(node, vm_writer, sym_tables):
	sym_tables["global"].class_name = node[1].content
	for child in node.children:
		if isinstance(child, parser.ParseTreeNode):
			compile_(child, vm_writer, sym_tables)

def _compile_subroutineDec(node, vm_writer, sym_tables):
	sym_tables["local"].clear()
	vm_writer.set_marker()

	func_type = node[0].content
	if func_type == "method":
		sym_tables["local"]._indexes["argument"] += 1

	compile_(node["parameterList"], vm_writer, sym_tables)

	if func_type == "method":
		vm_writer.push("argument", 0)
		vm_writer.pop("pointer", 0)

	elif func_type == "constructor":
		num_instance_vars = sym_tables["global"].num_symbols_by_type("field")
		vm_writer.push("constant", num_instance_vars)
		vm_writer.call("Memory.alloc", 1)
		vm_writer.pop("pointer", 0)

	compile_(node["subroutineBody"], vm_writer, sym_tables)

	num_local_vars = sym_tables["local"].num_symbols_by_type("var")
	vm_writer.goto_marker()
	func_name = sym_tables["global"].class_name + "." + node[2].content
	vm_writer.function(func_name, num_local_vars)

def _compile_parameterList(node, vm_writer, sym_tables):
	if node.children:
		sym_tables["local"].add_symbol(
			node[1].content, "argument", node[0].content
		)
		for arg_ind in range(3, len(node.children), 3):
			sym_tables["local"].add_symbol(
				node[arg_ind + 1].content, "argument", node[arg_ind].content
			)

def _compile_subroutineCall(node, vm_writer, sym_tables):
	num_args = node["expressionList"].count_nodes("expression")

	if len(node.children) == 6:
		sym_name = node.children[0].content
		try:
			object_sym = _get_symbol(sym_tables, sym_name)
			class_name = object_sym.kind
			num_args += 1
			vm_writer.push(object_sym.segment, object_sym.index)
		except KeyError:
			class_name = sym_name

		func_name = "".join([
			class_name, node.children[1].content,
			node.children[2].content
		])
	else:
		vm_writer.push("pointer", 0)
		num_args += 1
		func_name = sym_tables["global"].class_name + "." + \
			node.children[0].content

	compile_(node["expressionList"], vm_writer, sym_tables)
	vm_writer.call(func_name, num_args)

def _compile_term(node, vm_writer, sym_tables):
	is_token = isinstance(node.children[0], tokenizer.Token)
	if is_token and node.children[0].type_ == "integerConstant":
		vm_writer.push("constant", node.children[0].content)

	elif is_token and node.children[0].type_ == "stringConstant":
		string = node.children[0].content
		vm_writer.push("constant", len(string))
		vm_writer.call("String.new", 1)
		for char in string:
			vm_writer.push("constant", ord(char))
			vm_writer.call("String.appendChar", 2)

	elif is_token and node.children[0].type_ == "identifier":
		if len(node.children) == 4:
			sym = _get_symbol(sym_tables, node.children[0].content)
			vm_writer.push(sym.segment, sym.index)
			compile_(node.children[2], vm_writer, sym_tables)
			vm_writer.binary_op("+")
			vm_writer.pop("pointer", 1)
			vm_writer.push("that", 0)
		else:
			sym = _get_symbol(sym_tables, node.children[0].content)
			vm_writer.push(sym.segment, sym.index)

	else:
		for child in node.children:
			if isinstance(child, parser.ParseTreeNode):
				compile_(child, vm_writer, sym_tables)

def _compile_expression(node, vm_writer, sym_tables):
	compile_(node.children[0], vm_writer, sym_tables)
	for ind in xrange(1, len(node.children) - 1):
		compile_(node.children[ind + 1], vm_writer, sym_tables)
		vm_writer.binary_op(node.children[ind].content)

def _compile_keywordConstant(node, vm_writer, sym_tables):
	token = node[0].content
	if token == "true":
		vm_writer.push("constant", 0)
		vm_writer.unary_op("~")

	elif token == "false":
		vm_writer.push("constant", 0)

	elif token == "this":
		vm_writer.push("pointer", 0)

def _compile_letStatement(node, vm_writer, sym_tables):
	compile_(node[-2], vm_writer, sym_tables)
	sym = _get_symbol(sym_tables, node.children[1].content)
	if len(node.children) == 8:
		vm_writer.push(sym.segment, sym.index)
		compile_(node.children[3], vm_writer, sym_tables)
		vm_writer.binary_op("+")
		vm_writer.pop("pointer", 1)
		vm_writer.pop("that", 0)

	else:
		vm_writer.pop(sym.segment, sym.index)

def _compile_doStatement(node, vm_writer, sym_tables):
	compile_(node["subroutineCall"], vm_writer, sym_tables)
	vm_writer.pop("temp", 0)

def _compile_unaryExpression(node, vm_writer, sym_tables):
	compile_(node.children[1], vm_writer, sym_tables)
	vm_writer.unary_op(node.children[0].content)

def _compile_ifStatement(node, vm_writer, sym_tables):
	label_uid = str(sym_tables["global"].label_uid)
	else_label = "if_else_" + label_uid
	end_label = "if_end_" + label_uid
	sym_tables["global"].label_uid += 1

	compile_(node["expression"], vm_writer, sym_tables)
	vm_writer.unary_op("~")
	vm_writer.if_goto(else_label)
	compile_(node["statements"], vm_writer, sym_tables)
	vm_writer.goto(end_label)
	vm_writer.label(else_label)

	if len(node.children) > 7:
		compile_(node[9], vm_writer, sym_tables)

	vm_writer.label(end_label)

def _compile_whileStatement(node, vm_writer, sym_tables):
	label_uid = str(sym_tables["global"].label_uid)
	sym_tables["global"].label_uid += 1

	start_label = "while_start_" + label_uid
	end_label = "while_end_" + label_uid

	vm_writer.label(start_label)
	compile_(node["expression"], vm_writer, sym_tables)
	vm_writer.unary_op("~")
	vm_writer.if_goto(end_label)
	compile_(node["statements"], vm_writer, sym_tables)
	vm_writer.goto(start_label)
	vm_writer.label(end_label)

def _compile_returnStatement(node, vm_writer, sym_tables):
	if "expression" in node:
		compile_(node["expression"], vm_writer, sym_tables)
	else:
		vm_writer.push("constant", 0)
	vm_writer.return_()

def _compile_varDec(node, vm_writer, sym_tables):
	for var in node.filter_token(",").children[2:-1]:
		sym_tables["local"].add_symbol(
			var.content, "var", node[1].content
		)

def _compile_classVarDec(node, vm_writer, sym_tables):
	for varToken in node.filter_token(",")[2:-1]:
		sym_tables["global"].add_symbol(
			varToken.content, node[0].content, node[1].content
		)

def _get_symbol(sym_tables, sym_name):
	sym_scope = "local" if sym_name in sym_tables["local"] else "global"
	return sym_tables[sym_scope][sym_name]
