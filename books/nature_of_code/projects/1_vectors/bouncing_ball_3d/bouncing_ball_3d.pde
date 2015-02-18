/**
 * An animation of a sphere bouncing around a box, tracing its path as it
 * travels.
 */

import peasy.*;

/**
 * A 3-dimensional vector.
 */
class Vec3 {
	float x, y, z;

	Vec3(float x_, float y_, float z_){
		x = x_;
		y = y_;
		z = z_;
	}

	void add(Vec3 vector){
		x += vector.x;
		y += vector.y;
		z += vector.z;
	}
}

PeasyCam cam;

Vec3 sphereLoc = new Vec3(200, 200, 200);
Vec3 sphereVelocity = new Vec3(5.3, 7.5, 4);

Vec3 boxLoc = new Vec3(100, 100, 100);
Vec3 boxDimensions = new Vec3(200, 200, 200);

Vec3[] collisions = new Vec3[20];
int numCollisions = 0;

void setup(){
	size(1200, 800, P3D);
	background(255);
	cam = new PeasyCam(this, 300);
	cam.setMinimumDistance(50);
	cam.setMaximumDistance(500);
}

void draw(){
	background(255);

	sphereLoc.add(sphereVelocity);
	if((sphereLoc.x > boxDimensions.x) || (sphereLoc.x < 0)){
		sphereVelocity.x *= -1;
		storeCollision();
	}

	if((sphereLoc.y > boxDimensions.y) || (sphereLoc.y < 0)){
		sphereVelocity.y *= -1;
		storeCollision();
	}

	if((sphereLoc.z > boxDimensions.z) || (sphereLoc.z < 0)){
		sphereVelocity.z *= -1;
		storeCollision();
	}

	/**
	 * Trace the sphere's path from collision to collision with an
	 * exponentially fading color.
	 */
	for(int coll = 0; coll < numCollisions - 1; coll++){
		int fadeColor = (numCollisions - coll);
		fadeColor *= fadeColor;
		if(fadeColor > 210){
			fadeColor = 210;
		}

		stroke(255, fadeColor, fadeColor);
		Vec3 start = collisions[coll];
		Vec3 end = collisions[coll + 1];
		line(start.x, start.y, start.z, end.x, end.y, end.z);
	}

	if(numCollisions > 0){
		Vec3 lastColl = collisions[numCollisions - 1];
		line(
			lastColl.x, lastColl.y, lastColl.z,
			sphereLoc.x, sphereLoc.y, sphereLoc.z
		);
	}

	stroke(0);
	noFill();
	pushMatrix();
	translate(boxLoc.x, boxLoc.y, boxLoc.z);
	box(boxDimensions.x, boxDimensions.y, boxDimensions.z);
	point(boxDimensions.x, boxDimensions.y, boxDimensions.z);
	popMatrix();

	fill(255);
	pushMatrix();
	translate(sphereLoc.x, sphereLoc.y, sphereLoc.z);
	sphere(1);
	popMatrix();
}

/**
 * Store the current `sphereLoc` in `collisions`; expand the latter as needed.
 */
void storeCollision(){
	collisions[numCollisions++] = new Vec3(
		sphereLoc.x, sphereLoc.y, sphereLoc.z
	);
	if(numCollisions == collisions.length){
		collisions = (Vec3 [])expand(collisions);
	}
}
