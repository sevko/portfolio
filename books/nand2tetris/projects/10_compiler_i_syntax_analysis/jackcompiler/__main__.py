"""
Command-line entry point for the jackcompiler.
"""

import os
import sys
import jackcompiler

def compile_file(file_path):
	"""
	Compile a single Jack file into a file with an identical basename but an
	`xml` extension in the current directory.

	Args:
		file_path (string): The path to a Jack file.
	"""

	output_path = os.path.splitext(os.path.basename(file_path))[0] + ".xml"
	with open(file_path) as input_file:
		with open(output_path, "w") as output_file:
			output_file.write(jackcompiler.compile_(input_file.read()))

def compile_path(path):
	"""
	Args:
		path (string): The path to either a directory whose files are all
			assumed to be Jack files, or a single Jack file, to be compiled
			with `compile_file()`.
	"""

	if os.path.isfile(path):
		compile_file(path)

	elif os.path.isdir(path):
		for fpath in os.listdir(path):
			full_fpath = os.path.join(path, fpath)
			if os.path.isfile(full_fpath):
				compile_file(full_fpath)

compile_path(sys.argv[1])
