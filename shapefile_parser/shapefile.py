"""
A module for ESRI Shapefile reading, and writing.
"""

import sys

class Shapefile(object):

	def __init__(self):
		pass

class Reader(object):

	def __init__(self):
		pass

class Writer(object):

	def __init__(self):
		pass

if __name__ == "__main__":
	if 1 < len(sys.argv):
		print "Reading %s." % sys.argv[1]
	else:
		print (
			"Missing argument. Use:\n"
			"\tpython shapefile_parser.py SHAPEFILE_PATH\n\n"
			"\tSHAPEFILE_PATH : the path of a shapefile (.shp) file.")
