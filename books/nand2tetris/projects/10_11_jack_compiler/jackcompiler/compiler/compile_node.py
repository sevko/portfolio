from jackcompiler.syntax import parser

def compile_(node, vm_writer, sym_table):
	compile_func_name = "_compile_" + node.name
	globals_dict = globals()
	if compile_func_name in globals_dict:
		globals_dict[compile_func_name](node.children, vm_writer, sym_table)
	else:
		for child in node.children:
			if isinstance(child, parser.ParseTreeNode):
				compile_(child, vm_writer, sym_table)

class Tmp(object):

	def __init__(self, body):
		self.body = body

	def to_xml(self):
		return str(self.body)

def _compile_varDec(children, vm_writer, sym_tables):
	sym_tables["local"].add_symbol(
		children[2].content, children[1].content, children[0].content
	)

	sym = sym_tables["local"][children[2].content]
	children.append(Tmp(sym.name))
	children.append(Tmp(sym.type_))
	children.append(Tmp(sym.kind))
	children.append(Tmp(sym.index))

def _compile_classVarDec(children, vm_writer, sym_tables):
	sym_tables["global"].add_symbol(
		children[2].content, children[1].content, children[0].content
	)

	sym = sym_tables["global"][children[2].content]
	children.append(Tmp(sym.name))
	children.append(Tmp(sym.type_))
	children.append(Tmp(sym.kind))
	children.append(Tmp(sym.index))
