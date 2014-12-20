from jackcompiler.syntax import parser
from jackcompiler.syntax import tokenizer

def compile_(node, vm_writer, sym_tables):
	print("> " + node.name)
	compile_func_name = "_compile_" + node.name
	globals_dict = globals()
	if compile_func_name in globals_dict:
		globals_dict[compile_func_name](node, vm_writer, sym_tables)
	else:
		for child in node.children:
			if isinstance(child, parser.ParseTreeNode):
				compile_(child, vm_writer, sym_tables)

def _compile_class(node, vm_writer, sym_tables):
	sym_tables["global"].class_name = node[1].content
	for child in node.children:
		if isinstance(child, parser.ParseTreeNode):
			compile_(child, vm_writer, sym_tables)

def _compile_subroutineDec(node, vm_writer, sym_tables):
	sym_tables["local"].clear()
	vm_writer.set_marker()
	compile_(node["parameterList"], vm_writer, sym_tables)
	compile_(node["subroutineBody"], vm_writer, sym_tables)

	num_local_vars = sym_tables["local"].num_symbols_by_type("var")
	vm_writer.goto_marker()
	func_name = sym_tables["global"].class_name + "." + node[2].content
	vm_writer.function(func_name, num_local_vars)

def _compile_parameterList(node, vm_writer, sym_tables):
	if node.children:
		sym_tables["local"].add_symbol(
			node[1].content, node[0].content, "argument"
		)
		for arg_ind in range(3, len(node.children), 3):
			sym_tables["local"].add_symbol(
				node[arg_ind + 1].content, node[arg_ind].content, "argument"
			)

def _compile_subroutineCall(node, vm_writer, sym_tables):
	if len(node.children) == 6:
		func_name = "".join([
			node.children[0].content, node.children[1].content,
			node.children[2].content
		])
	else:
		func_name = node.children[0].content
	num_args = node["expressionList"].count_nodes("expression")

	compile_(node["expressionList"], vm_writer, sym_tables)
	vm_writer.call(func_name, num_args)

def _compile_term(node, vm_writer, sym_tables):
	is_token = isinstance(node.children[0], tokenizer.Token)
	if is_token and node.children[0].type_ == "integerConstant":
		vm_writer.push("constant", node.children[0].content)
	elif is_token and node.children[0].type_ == "identifer":
		sym = sym_tables[node.children[0].content]
		vm_writer.push(sym.type_, sym.index)
	else:
		for child in node.children:
			if isinstance(child, parser.ParseTreeNode):
				compile_(child, vm_writer, sym_tables)

def _compile_expression(node, vm_writer, sym_tables):
	compile_(node.children[0], vm_writer, sym_tables)
	for ind in xrange(1, len(node.children) - 1):
		compile_(node.children[ind + 1], vm_writer, sym_tables)
	vm_writer.binary_op(node.children[ind].content)

def _compile_stringConstant(node, vm_writer, sym_tables):
	pass

def _compile_keywordConstant(node, vm_writer, sym_tables):
	pass

def _compile_varName(node, vm_writer, sym_tables):
	pass

def _compile_unaryExpression(node, vm_writer, sym_tables):
	compile_(node.children[0])
	vm_writer.unary_op(node.children[0].content)

def _compile_returnStatement(node, vm_writer, sym_tables):
	vm_writer.return_()

def _compile_varDec(node, vm_writer, sym_tables):
	for var in node.filter_token(",").children[2:-1]:
		sym_tables["local"].add_symbol(
			var.content, node[1].content, "var"
		)

def _compile_classVarDec(node, vm_writer, sym_tables):
	sym_tables["global"].add_symbol(
		node[2].content, node[1].content, node[0].content
	)
