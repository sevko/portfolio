from jackcompiler.compiler import vmwriter
from jackcompiler.compiler import symbol_table
from jackcompiler.compiler import compile_node

def compile_cst(cst):
	"""
	Compile a Jack Concrete Syntax Tree into VM code.

	Args:
		cst (`parser.ParseTreeNode`): The root node of the parse tree of a Jack
			source file (should be the `class` node).

	Returns:
		(string) The Hack Virtual Machine code representation of the `cst`.
	"""

	vm_writer = vmwriter.VMWriter()
	sym_tables = {
		"local": symbol_table.SymbolTable(),
		"global": symbol_table.SymbolTable()
	}
	compile_node.compile_(cst, vm_writer, sym_tables)
	return vm_writer.get_code()
