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
		shape_type (int) : Any of the class constants defined in this class.
	"""

	NULL_SHAPE = 0
	POINT = 1
	MULTI_POINT = 8
	POLYGON = 5

	def __init__(self, shape_type):
		self.shape_type = shape_type

class NullShape(Shape):
	"""
	A Shapefile Null Shape.
	"""

	def __init__(self):
		super(NullShape, self).__init__(Shape.NULL_SHAPE)

	def to_binary(self):
		"""
		Convert a `NullShape` to its binary representation.

		Returns:
			str: `self` in the bit-packed binary format described by the
			Shapefile specification for `NullShape`:

			Byte | Field | Type | Number | Endianness
			0 | `self.shape_type`  | Integer | 1 | Little
			4 | `self.x` | Double | 1 | Little
			12 | `self.y` | Double | 1 | Little
		"""

		return struct.pack("<i", self.shape_type)

class Point(Shape):
	"""
	A Shapefile Point object.

	Attributes:
		x (double): The point's x-coordinate.
		y (double): The point's y-coordinate.
	"""

	def __init__(self, x, y):
		super(Point, self).__init__(Shape.POINT)
		self.x = x
		self.y = y

	@classmethod
	def from_binary(cls, bin_str):
		"""
		Recreate a `Point` instance from its compressed binary form.

		Args:
			bin_str (str) : A bit-packed representation of a `Point`, as
				written by `Point.to_binary()`.

		Returns:
			Point: A `Point` instance with member values as read from
				`bin_str`.
		"""

		return Point(*struct.unpack("<2d", bin_str))

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

		return struct.pack("<i2d", self.shape_type, self.x, self.y)

class MultiPoint(Shape):
	"""
	A Shapefile MultiPoint object.

	Attributes:
		bounding_box (BoundingBox): The MultiPoint's bounding box.
		points (list of Point): The constituent points.
	"""

	def __init__(self, bounding_box, points):
		super(MultiPoint, self).__init__(Shape.MULTI_POINT)
		self.bounding_box = bounding_box
		self.points = points

	@classmethod
	def from_binary(cls, bin_str):
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

		return "%s%s%s%s" % (
			struct.pack("<i", self.shape_type),
			self.bounding_box.to_binary(),
			struct.pack("<i", len(self.points)),
			"".join([pt.to_binary() for pt in self.points]))

class Polygon(Shape):
	"""
	A Shapefile Polygon object.

	Attributes:
		bounding_box (BoundingBox): The Polygon's bounds.
		parts (list of ints): The indexes of the first Point of each "part" of
			the Polygon in self.points.
		points (list of Point): The constituent points of the Polygon.
	"""

	def __init__(self, bounding_box, parts, points):
		super(Polygon, self).__init__(Shape.POLYGON)
		self.bounding_box = bounding_box
		self.parts = parts
		self.points = points

	@classmethod
	def from_binary(cls, bin_str):
		"""
		Recreate a 'Polygon' instance from its compressed binary form.

		Args:
			bin_str (str): A bit-packed representation of a `Polygon`, as
				written by `Polygon.to_binary()`.

		Returns:
			Polygon: A `Polygon` instance with its member values as read from
			`bin_str`.
		"""

		bounding_box = BoundingBox(*struct.unpack("<4d", bin_str[:4 * 8]))

		num_parts = struct.unpack("<i", bin_str[32:36])[0]
		num_points = struct.unpack("<i", bin_str[36:40])[0]
		parts = struct.unpack("<%di" % num_parts,
			bin_str[40:40 + 4 * num_parts])

		bin_str = bin_str[40 + 4 * num_parts:]
		point_bin_strs = [bin_str[pt * 16:pt * 16 + 16] for pt in
			xrange(num_points)]
		points = [Point.from_binary(binstr) for binstr in point_bin_strs]

		return Polygon(bounding_box, parts, points)

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

		return "".join([
			struct.pack("<i", self.shape_type),
			self.bounding_box.to_binary(),
			struct.pack(
				"<%di" % (2 + len(self.parts)), len(self.parts),
				len(self.points), *self.parts),
			"".join([pt.to_binary()[4:] for pt in self.points])
		])

class Shapefile(object):
	"""
	A Shapefile representation.

	Attributes:
		shape_type (int): Any of the constant instances in `Shape`.
		length (int): The number of bytes in the file.
		bounding_box (BoundingBox): The shapefile's bounds.
		shapes (list of Shape): The `Shape`s contained in this `Shapefile`.
	"""

	def __init__(self, filename=None, length=None, shape_type=None,
			bounding_box=None, shapes=None):
		self.length = length
		self.shape_type = shape_type
		self.bounding_box = bounding_box
		self.shapes = shapes

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
				" convention.", path)

		with open(path) as shapefile:
			def read_and_unpack(fmt, num_bytes):
				"""
				Read and unpack bytes from the `shapefile` file object.
				"""

				return struct.unpack(fmt, shapefile.read(num_bytes))

			shapefile.seek(24)
			self.length = read_and_unpack(">i", 4)[0] * 2
			shapefile.seek(32)
			self.shape_type = read_and_unpack("<i", 4)[0]
			self.bounding_box = BoundingBox(*read_and_unpack("<8d", 64))
			self.shapes = []

			print "Length: %d" % self.length
			print "Shape type: %d" % self.shape_type

			def read_header_content_len():
				"""
				Read the shape-type from the next few bytes in the `shapefile`
				file object.
				"""

				shapefile.seek(4, 1)
				return shapefile.read(4)

			shape_types = {
				Shape.NULL_SHAPE : NullShape,
				Shape.POINT : Point,
				Shape.MULTI_POINT : MultiPoint,
				Shape.POLYGON : Polygon
			}

			content_len_str = read_header_content_len()
			while content_len_str:
				content_len = struct.unpack(">i", content_len_str)[0] * 2
				shape_type = struct.unpack("<i", shapefile.read(4))[0]
				shape_bin_str = shapefile.read(content_len - 4)

				self.shapes.append(
					shape_types[shape_type].from_binary(shape_bin_str))
				content_len_str = read_header_content_len()

	def write_to_file(self, path):
		"""
		Write this Shapefile instance to a file.

		The fields of this object are written in the bit-packed format
		described by the Shapefile specification.

		Args:
			path (str): The path of the file to write.
		"""

		with open(path, "w") as shapefile:
			header = "".join([
				struct.pack(">7i", 9994, 0, 0, 0, 0, 0, self.length / 2),
				struct.pack("<2i", 1000, self.shape_type),
				self.bounding_box.to_binary(True)
			])

			record_strs = []
			for rec_num in xrange(len(self.shapes)):
				rec_body = self.shapes[rec_num].to_binary()
				rec_header = struct.pack(
					">2i", rec_num + 1, len(rec_body) / 2)
				record_strs.append("%s%s" % (rec_header, rec_body))

			shapefile.write("%s%s" % (header, "".join(record_strs)))

	@classmethod
	def _valid_filename(cls, path):
		"""
		Verify whether a shapefile filename satisfies the 8.3 convention.

		Args:
			path (str): The path of a shapefile.

		Returns:
			bool: True if the shapefile's filename satisfies the 8.3 filename
			convention (the root is 8 characters in length, starts with
			[a-Z0-9], and continues with [a-Z0-9_-], and the
			extension is 3 characters long); False otherwise.
		"""

		filename = path.split("/")[-1].split(".")
		return (
			re.compile("^[a-zA-Z0-9][a-zA-Z0-9_-]{7}$").match(filename[0])
			and filename[1] == "shp")

class BoundingBox(object):
	"""
	A Shapefile bounding box, which specifies the goemetric bounds for
	`Shape`s.

	Attributes:
		min_x (double): The minimum x-coordinate in the parent `Shape.`
		min_y (double): The minimum y-coordinate in the parent `Shape.`
		max_x (double): The maximum x-coordinate in the parent `Shape.`
		max_y (double): The maximum y-coordinate in the parent `Shape.`
		min_m (double): The minimum m-coordinate in the parent `Shape.`
		max_m (double): The maximum m-coordinate in the parent `Shape.`
		min_z (double): The minimum z-coordinate in the parent `Shape.`
		max_z (double): The maximum z-coordinate in the parent `Shape.`
	"""

	def __init__(self, min_x, min_y, max_x, max_y, min_m=None, max_m=None,
			min_z=None, max_z=None):
		self.min_x = min_x
		self.max_x = max_x
		self.min_y = min_y
		self.max_y = max_y
		self.min_m = min_m
		self.max_m = max_m
		self.min_z = min_z
		self.max_z = max_z

	def to_binary(self, all_=False):
		"""
		Convert `self` to its binary form.

		Args:
			all_ (bool): If True, include the `BoundingBox`'s `z` and `m`
				values in the output: this is only necessary for a
				`Shapefile`'s global bounding box, and those of `Shape`
				classes supporting `z` and `m` measures (not implemented in
				this module).

		Return:
			A bit-packed binary string describing this `BoundingBox` as per the
			Shapefile specification.

			Byte | Field | Type
			0 | self.min_x | double
			8 | self.min_y | double
			16 | self.max_x | double
			24 | self.max_y | double
			32 | self.min_z | double
			40 | self.max_z | double
			48 | self.min_m | double
			56 | self.max_m | double
		"""

		bin_str = struct.pack(
				"<4d", self.min_x, self.min_y, self.max_x, self.max_y)
		if all_:
			for attribute in [self.min_z, self.max_z, self.min_m, self.max_m]:
				bin_str += struct.pack("<d", attribute if attribute else 0)
		return bin_str

def _handle_command_line_args():
	"""
	Handles and prints diagnostic messages for command-line arguments.
	"""

	def fatal_error(msg):
		logging.critical("%s. Use `--help` for use instructions.", msg)
		exit(1)

	usage_instructions = (
		"Missing argument. Use:\n"
		"\tpython shapefile_parser.py"
		"(--test | --file SHAPEFILE_PATH | --help)\n\n"
		"\t--test : run unit-tests.\n"
		"\t--file : Read a shapefile.\n"
		"\t\tSHAPEFILE_PATH : the path of a shapefile (.shp) file.\n"
		"\t--help : Print usage information.")

	if 1 < len(sys.argv):
		if sys.argv[1] == "--test":
			logging.info("Running unit-tests.")
			_unit_tests()

		elif sys.argv[1] == "--file":
			if len(sys.argv) < 3:
				fatal_error("`--file` flag requires a file-path parameter.")

			elif len(sys.argv) > 3:
				fatal_error("Too many arguments for the `--file` flag.")
			path = os.path.expanduser(sys.argv[2])
			logging.info("Reading shapefile %s.", path)
			shapefile = Shapefile(path)
			shapefile.write_to_file("a.shp")

		elif sys.argv[1] == "--help":
			print usage_instructions

		else:
			fatal_error("`%s` flag not recognized." % sys.argv[1])

	else:
		fatal_error(usage_instructions)

def _unit_tests():
	"""
	Execute the module's unit-tests.
	"""

	shapefile = Shapefile("test/test.shp")
	accepted_shapefile = {
		"length" : 22444,
		"shape_type" : 1,
	}

	accepted_bounding_box = {
		"min_x" : 13.564105429550633,
		"max_x" : 50.9698128,
		"min_y" : 13.9785217,
		"max_y" : 51.1843726,
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

	logging.basicConfig(
		format="%(levelname)s: %(funcName)s: %(message)s",
		level=logging.INFO)

if __name__ == "__main__":
	_configure_logging()
	_handle_command_line_args()
