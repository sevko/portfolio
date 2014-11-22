import sys

from assembler import assemble

def handle_user_args(argv):
	with open(argv[1]) as code_file:
		code = code_file.read()

	for line in assemble.assemble_code(code):
		print line

handle_user_args(sys.argv)
