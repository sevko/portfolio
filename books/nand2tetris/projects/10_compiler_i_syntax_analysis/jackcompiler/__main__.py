"""
WIP.
"""

import sys
import jackcompiler

with open(sys.argv[1]) as file_:
	for item in jackcompiler.compile(file_.read()):
		print(item)
