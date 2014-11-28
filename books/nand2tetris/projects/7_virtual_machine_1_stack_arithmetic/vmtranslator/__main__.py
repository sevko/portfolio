"""
Main entry point for the vmtranslator package when executed from the command
line; compiles Hack Virtual Machine program files to a hack assembly program.
"""

import sys

from vmtranslator import translator

def translate_file(path):
	with open(path) as vm_file:
		print(translator.translate(vm_file.read()))

if __name__ == "__main__":
	translate_file(sys.argv[1])
