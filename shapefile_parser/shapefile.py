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

	Attributes:
		points (list of doubles) : All of the points belonging to this shape.
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

class NullShape(object):
	"""
	A Shapefile Null Shape.

	Attributes:
		shape_type (int): See `Shape.NULL_SHAPE`.
	"""

	def __init__(self):
		self.shape_type = Shape.NULL_SHAPE

class Point(object):
	"""
	A Shapefile Point object.

	Attributes:
		shape_type (int) : See `Shape.POINT`.
		x (double): The point's x-coordinate.
		y (double): The point's y-coordinate.
	"""

	def __init__(self, x, y):
		self.shape_type = Shape.POINT
		self.x = x
		self.y = y

	def from_binary(self, bin_str):
		"""
		Recreate a `Point` instance from its compressed binary form.

		Args:
			bin_str (str) : A bit-packed representation of a `Point`, as
				written by `Point.to_binary()`.

		Returns:
			Point: A `Point` instance with member values as read from
				`bin_str`.
		"""

		return Point(*struct.unpack("<i<2d", bin_str)[1:])

	def to_binary(self):
		"""
		Convert a `Point` to its binary representation.

		Returns:
			str: `self` in the bit-packed binary format described by the
			Shapefile specification for `Point`:

			Byte | Field | Type | Number | Endianness
			0 | `self.shape_type`  | Integer | 1 | Little
			4 | `self.x` | Double | 1 | Little
			12 | `self.y` | Double | 1 | Little
		"""

		return struct.pack("<i<2d", self.shape_type, self.x, self.y)

class MultiPoint(object):
	"""
	A Shapefile MultiPoint object.

	Attributes:
		shape_type (int): See `Shape.MULTI_POINT`.
		bounding_box (BoundingBox): The MultiPoint's bounding box.
		points (list of Point): The constituent points.
	"""

	def __init__(self, bounding_box, points):
		self.bounding_box = bounding_box
		self.points = points

	def from_binary(self, bin_str):
		"""
		Recreate a `MultiPoint` instance from its compressed binary form.

		Args:
			bin_str (str): A bit-packed representation of a `MultiPoint`, as
			written by `MultiPoint.to_binary()`.

		Returns:
			MultiPoint: A `MultiPoint` instance with its member values as read
			from `bin_str`.
		"""

		bounding_box = BoundingBox(*struct.unpack("<4d", bin_str[4:4 + 4 * 8]))
		bin_str = bin_str[4 + 4 * 8:]
		num_points = struct.unpack("<i", bin_str[:4])
		bin_str = bin_str[4:]
		point_bin_strs = [bin_str[pt * 20:pt * 20 + 20] for pt in num_points]
		points = [Point.from_binary(bin_str) for bin_str in point_bin_strs]
		return MultiPoint(bounding_box, points)

	def to_binary(self):
		"""
		Convert a `MultiPoint` to its binary representation.

		Returns:
			str: `self` in the bit-packed binary format decsribed by the
			Shapefile specification for `MultiPoint`.

			Byte | Field | Type | Number | Endianness
			0 | self.shape_type | Integer | 1 | Little
			4 | self.bounding_box | Double | 4 | Little
			36 | len(self.points) | Integer | 1 | Little
			40 | self.points | Point | len(self.points) | Little
		"""

		return "%s%s" % struct.pack(
				"<i<4d<i", self.shape_type,
				*self.bounding_box.points, len(self.points)),
			"".join([point.to_binary() for pt in self.points]))

class Polygon(object):
	"""
	A Shapefile Polygon object.

	Attributes:
		shape_type (int): See Shape.POLY_LINE.
		bounding_box (BoundingBox): The Polygon's bounds.
		parts (list of ints): The indexes of the first Point of each "part" of
			the Polygon in self.points.
		points (list of Point): The constituent points of the Polygon.
	"""

	def __init__(self, bounding_box, parts, points):
		self.bounding_box = bounding_box
		self.parts = parts
		self.points = points

	def from_binary(self, bin_str):
		"""
		Recreate a 'Polygon' instance from its compressed binary form.

		Args:
			bin_str (str): A bit-packed representation of a `Polygon`, as
				written by `Polygon.to_binary()`.

		Returns:
			Polygon: A `Polygon` instance with its member values as read from
			`bin_str`.
		"""

		self.bounding_box = bounding_box(
			*struct.unpack("<4d", bin_str[4:4 + 4 * 8]))

		num_parts = struct.unpack("<i", bin_str[36:40])
		num_points = struct.unpack("<i", bin_str[40:44])
		self.parts = struct.unpack("<%di" % num_parts,
			bin_str[44:44 + 4 * num_parts])

		bin_str = bin_str[44 + 4 * num_parts:]
		point_bin_strs = [bin_str[pt * 20:pt * 20 + 20] for pt in num_points]
		self.points = [Point.from_binary(binstr) for binstr in point_bin_strs]

	def to_binary(self):
		"""
		Convert a `Polygon` to its binary representation.

		Returns:
			str: `self` in the bit-packed binary format described by the
			Shapefile specification for `Polygon`.

			Byte | Field | Type | Number | Endianness
			0 | Shape | Type | Integer | 1 | Little
			4 | Box | Double | 4 | Little
			36 | NumParts | Integer | 1 | Little
			40 | NumPoints | Integer | 1 | Little
			44 | Parts | Integer | NumParts | Little
			X | Points | Point | NumPoints | Little
		"""

		return "%s%s" % (struct.pack(
				"<i<4d<3i<%di" % len(self.parts), self.shape_type,
				self.bounding_box.points, len(self.parts), len(self.points),
				self.parts),
			"".join([pt.to_binary() for pt in self.points]))

class Shapefile(object):
	"""
	A Shapefile representation.

	Attributes:
		length (int): The number of bytes in the file.
		shape_type (int): Any of the constant instances in `Shape`.
		bounding_box (BoundingBox): The shapefile's bounds.
	"""

	def __init__(self, filename=None, length=None, shape_type=None,
			bounding_box=None):
		self.length = length
		self.shape_type = shape_type
		self.bounding_box = bounding_box

		if filename:
			self.read_from_file(filename)

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
			def read_and_unpack(fmt, num_bytes):
				return struct.unpack(fmt, shapefile.read(num_bytes))

			shapefile.seek(24)
			self.length = read_and_unpack(">i", 4)[0] * 2
			shapefile.seek(32)
			self.shape_type = read_and_unpack("<i", 4)[0]
			self.bounding_box = BoundingBox(*read_and_unpack("<8d", 64))

			shapefile.seek(4, 1)

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
		if sys.argv[1] == "--test":
			logging.info("Running unit-tests.")
			_unit_tests()
		elif sys.argv[1] == "--file":
			path = os.path.expanduser(sys.argv[2])
			logging.info("Reading shapefile %s." % path)
			shapefile = Shapefile(path)

	else:
		logging.critical(
			"Missing argument. Use:\n"
			"\tpython shapefile_parser.py (--test | --file SHAPEFILE_PATH)\n\n"
			"\t--test : run unit-tests.\n"
			"\t--script : Read a shapefile.\n"
			"\t\tSHAPEFILE_PATH : the path of a shapefile (.shp) file.")

def _unit_tests():
	shapefile = Shapefile("test/test.shp")
	accepted_shapefile = {
		"length" : 53139272,
		"shape_type" : 5,
	}

	accepted_bounding_box = {
		"min_x" : -159.79790283200012,
		"max_x" : -46.63492012129996,
		"min_y" : 178.14644409200054,
		"max_y" : 69.03239737290023,
		"min_z" : 0.0,
		"max_z" : 0.0,
		"min_m" : 0.0,
		"max_m" : 0.0
	}

	for member in accepted_shapefile:
		assert getattr(shapefile, member) == accepted_shapefile[member]


	for member in accepted_bounding_box:
		assert (getattr(shapefile.bounding_box, member) ==
				accepted_bounding_box[member])

def _configure_logging():
	"""
	Configure settings for the `logging` module.
	"""

	logging.basicConfig(format="%(levelname)s: %(funcName)s: %(message)s")

if __name__ == "__main__":
	_configure_logging()
	_handleCommandLineArgs()
