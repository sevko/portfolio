class Scene:

	def __init__(self, camera, objects, lights, width, height):
		pass

	def render(self):
		pass

	def _get_intersection(self, ray):
		pass

class Vector:

	def __init__(self, x, y, z):
		pass

Point = Vector
Color = Vector

class Sphere:

	def __init__(self):
		pass

	def intersects(self, ray):
		pass

class Ray:

	def __init__(self, origin, direction):
		pass

	def point_at_dist(self, dist):
		pass

def pixels_to_ppm(pixels):
	pass

if __name__ == "__main__":
	scene = Scene()
	pixels = scene.render()
	with open("image.ppm", "w") as img_file:
		img_file.write(pixels_to_ppm(pixels))
