"""
Command-line entry point for the jackcompiler.
"""

import os
import sys
import jackcompiler

def compile_file(file_path):
	output_path = os.path.splitext(os.path.basename(file_path))[0] + ".xml"
	with open(file_path) as input_file:
		with open(output_path, "w") as output_file:
			output_file.write(jackcompiler.compile_(input_file.read()))

def compile_path(path):
	if os.path.isfile(path):
		compile_file(path)

	elif os.path.isdir(path):
		for fpath in os.listdir(path):
			full_fpath = os.path.join(path, fpath)
			if os.path.isfile(full_fpath):
				compile_file(full_fpath)

compile_path(sys.argv[1])
