import math
import collections

class Scene:

	def __init__(self, camera, objects, lights, width, height):
		self.camera = camera
		self.objects = objects
		self.lights = lights
		self.width = width
		self.height = height

	def render(self):
		pixels = [
			[Color() for _ in range(self.width)] for _ in range(self.height)]

		for y in range(self.height):
			for x in range(self.width):
				ray_direction = Point(x, y) - self.camera
				ray = Ray(self.camera, ray_direction)
				pixels[y][x] = self._trace_ray(ray)

		return pixels

	def _trace_ray(self, ray, depth=0, max_depth=5):
		color = Color()

		if depth >= max_depth:
			return color

		intersection = self._get_intersection(ray)
		if intersection is None:
			return color

		obj, dist = intersection
		intersection_pt = ray.point_at_dist(dist)
		surface_norm = obj.surface_norm(intersection_pt)

		# ambient
		color += obj.material.color * obj.material.ambient

		# lambert
		for light in self.lights:
			pt_to_light_vec = (light - intersection_pt).normalize()
			pt_to_light_ray = Ray(intersection_pt, pt_to_light_vec)
			if self._get_intersection(pt_to_light_ray) is None:
				lambert_intensity = surface_norm * pt_to_light_vec
				if lambert_intensity > 0:
					color += obj.material.color * obj.material.lambert * \
						lambert_intensity

		# specular
		reflected_ray = Ray(
			intersection_pt, ray.direction.reflect(surface_norm).normalize())
		color += self._trace_ray(reflected_ray, depth + 1) * \
			obj.material.specular
		return color

	def _get_intersection(self, ray):
		intersection = None
		for obj in self.objects:
			dist = obj.intersects(ray)
			if dist is not None and \
				(intersection is None or dist < intersection[1]):
				intersection = obj, dist

		return intersection

class Vector:

	def __init__(self, x=0, y=0, z=0):
		self.x = x
		self.y = y
		self.z = z

	def norm(self):
		return math.sqrt(sum(num * num for num in self))

	def normalize(self):
		return self / self.norm()

	def reflect(self, other):
		other = other.normalize()
		return self - 2 * (self * other) * other

	def __str__(self):
		return "Vector({}, {}, {})".format(*self)

	def __add__(self, other):
		return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

	def __sub__(self, other):
		return Vector(self.x - other.x, self.y - other.y, self.z - other.z)

	def __mul__(self, other):
		if isinstance(other, Vector):
			return self.x * other.x + self.y * other.y + self.z * other.z;
		else:
			return Vector(self.x * other, self.y * other, self.z * other)

	def __rmul__(self, other):
		return self.__mul__(other)

	def __truediv__(self, other):
		return Vector(self.x / other, self.y / other, self.z / other)

	def __pow__(self, exp):
		if exp != 2:
			raise ValueError("Exponent can only be two")
		else:
			return self * self

	def __iter__(self):
		yield self.x
		yield self.y
		yield self.z

Point = Vector
Color = Vector

class Sphere:

	def __init__(self, origin, radius, material):
		self.origin = origin
		self.radius = radius
		self.material = material

	def intersects(self, ray):
		sphere_to_ray = ray.origin - self.origin
		b = 2 * ray.direction * sphere_to_ray
		c = sphere_to_ray ** 2 - self.radius ** 2
		discriminant = b ** 2 - 4 * c

		if discriminant >= 0:
			dist = (-b - math.sqrt(discriminant)) / 2
			if dist > 0:
				return dist

	def surface_norm(self, pt):
		return (pt - self.origin).normalize()

Material = collections.namedtuple("Material", "color specular lambert ambient")

class Material:

	def __init__(self, color, specular=0.5, lambert=1, ambient=0.2):
		self.color = color
		self.specular = specular
		self.lambert = lambert
		self.ambient = ambient

class Ray:

	def __init__(self, origin, direction):
		self.origin = origin
		self.direction = direction.normalize()

	def point_at_dist(self, dist):
		return self.origin + self.direction * dist

def pixels_to_ppm(pixels):
	header = "P3 {} {} 255\n".format(len(pixels[0]), len(pixels))
	img_data_rows = []
	for row in pixels:
		pixel_strs = [
			" ".join([str(int(color)) for color in pixel]) for pixel in row]
		img_data_rows.append(" ".join(pixel_strs))
	return header + "\n".join(img_data_rows)

if __name__ == "__main__":
	objects = [
		Sphere(
			Point(150, 120, -20), 80, Material(Color(0xFF, 0, 0),
			specular=0.2)),
		Sphere(
			Point(420, 120, 0), 100, Material(Color(0, 0, 0xFF),
			specular=0.8)),
		Sphere(Point(320, 240, -40), 50, Material(Color(0, 0xFF, 0))),
		Sphere(
			Point(300, 200, 200), 100, Material(Color(0xFF, 0xFF, 0),
			specular=0.8)),
		Sphere(Point(300, 130, 100), 40, Material(Color(0xFF, 0, 0xFF))),
		Sphere(Point(300, 1000, 0), 700, Material(Color(0xFF, 0xFF, 0xFF),
			lambert=0.5)),
		]
	lights = [Point(200, -100, 0)]
	camera = Point(200, 200, -400)
	scene = Scene(camera, objects, lights, 600, 400)
	pixels = scene.render()
	with open("image.ppm", "w") as img_file:
		img_file.write(pixels_to_ppm(pixels))
