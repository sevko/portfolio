"""
A module for ESRI Shapefile reading and writing.
"""

import logging
import os
import re
import struct
import sys

class Shape(object):
	"""
	Contains Shapefile shape types.
	"""

	NULL_SHAPE = 0
	POINT = 1
	POLY_LINE = 3
	POLYGON = 5
	MULTI_POINT = 8
	POINT_Z = 11
	POLY_LINE_Z = 13
	POLYGON_Z = 15
	MULTI_POINT_Z = 18
	POINT_M = 21
	POLY_LINE_M = 23
	POLYGON_M = 25
	MULTI_POINT_M = 28
	MULTI_PATCH = 31

class Shapefile(object):
	"""
	A Shapefile representation.

	Attributes:
		length (int): The number of bytes in the file.
		shape_type (int): Any of the constant instances in `Shape`.
		bounding_box (BoundingBox): The shapefile's bounds.
	"""

	def __init__(self, length=None, shape_type=None, bounding_box=None):
		self.length = length
		self.shape_type = shape_type
		self.bounding_box = bounding_box

	def read_from_file(self, path):
		"""
		Load a shapefile file into this Shapefile instance.

		Args:
			path (str): The path of a shapefile; tildes, "~", are valid.
		"""

		path = os.path.expanduser(path)
		if not self._valid_filename(path):
			logging.warning(
				"Shapefile filename '%s' does not satisfy 8.3 filename"
				" convention." % path)

		with open(path) as shapefile:
			shapefile.seek(24)
			self.length = struct.unpack(">i", shapefile.read(4))[0]
			shapefile.seek(32)
			self.shape_type = struct.unpack("<i", shapefile.read(4))[0]
			self.bounding_box = BoundingBox(
				*struct.unpack("<8d", shapefile.read(64)))

	def write_to_file(self, path):
		pass

	def _valid_filename(self, path):
		"""
		Verify whether a shapefile filename satisfies the 8.3 convention.

		Args:
			path (str): The path of a shapefile.

		Returns:
			bool: True if the shapefile's filename satisfies the 8.3 filename
			convention (the root is 8 characters in length, starts with
			[a-Z0-9], and continues with [a-Z0-9_-]; False otherwise, and the
			extension is 3 characters long).
		"""

		filename = path.split("/")[-1].split(".")
		return (
			re.compile("^[a-zA-Z0-9][a-zA-Z0-9_-]{7}$").match(filename[0])
			and filename[1] == "shp")

class BoundingBox(object):
	def __init__(self, min_x, max_x, min_y, max_y, min_m=None, max_m=None,
			min_z=None, max_z=None):
		self.min_x = min_x
		self.max_x = max_x
		self.min_y = min_y
		self.max_y = max_y
		self.min_m = min_m
		self.max_m = max_m
		self.min_z = min_z
		self.max_z = max_z

def _handleCommandLineArgs():
	"""
	Handles and prints diagnostic messages for command-line arguments.
	"""

	if 1 < len(sys.argv):
		shapefile = Shapefile()
		path = os.path.expanduser(sys.argv[1])
		shapefile.read_from_file(path)
		logging.info("Reading shapefile %s." % path)
	else:
		logging.critical(
			"Missing argument. Use:\n"
			"\tpython shapefile_parser.py SHAPEFILE_PATH\n\n"
			"\tSHAPEFILE_PATH : the path of a shapefile (.shp) file.")

def _configure_logging():
	"""
	Configure settings for the `logging` module.
	"""

	logging.basicConfig(format="%(levelname)s %(funcName)s %(message)s")

if __name__ == "__main__":
	_configure_logging()
	_handleCommandLineArgs()
