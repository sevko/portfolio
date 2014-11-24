"""
The main entry point for the Hack Assembler package, should it be executed
directly; assembles a source file to machine code.
"""

import sys
import os

from assembler import assemble

def handle_user_args(argv):
	"""
	Handle user command-line arguments.

	Args:
		argv (list of string): The arguments passed to the module on the
			command line. Should have only one: the path to a Hack assembly
			file, which will be compiled into a raw machine code file with an
			identical basename.
	"""

	file_path = argv[0]
	with open(file_path) as code_file:
		code = code_file.read()

	output = "\n".join([line for line in assemble.assemble_code(code)])
	output_path = os.path.splitext(os.path.basename(file_path))[0] + ".hack"
	with open(output_path, "w") as output_file:
		output_file.write(output + "\n")

handle_user_args(sys.argv[1:])
