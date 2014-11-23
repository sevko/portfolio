import sys
import os

from assembler import assemble

def handle_user_args(argv):
	file_path = argv[1]
	with open(file_path) as code_file:
		code = code_file.read()

	output = "\n".join([line for line in assemble.assemble_code(code)])
	output_path = os.path.splitext(os.path.basename(file_path))[0] + ".hack"
	with open(output_path, "w") as output_file:
		output_file.write(output + "\n")

handle_user_args(sys.argv)
