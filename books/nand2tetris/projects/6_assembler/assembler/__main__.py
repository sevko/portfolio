import sys

from assembler import assemble

def handle_user_args(argv):
	with open(argv[1]) as code_file:
		print assemble.assemble_code(code_file.read())

handle_user_args(sys.argv)
