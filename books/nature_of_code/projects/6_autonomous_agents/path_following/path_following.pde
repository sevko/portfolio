/**
 * A sketch that renders a vehicle following a path.
 */

Path path;
Vehicle vehicle;

void setup(){
	size(1000, 800);
}

void mouseClicked(){
	if(start == null){
		start = new PVector(mouseX, mouseY);
	}
	else {
		PVector end = new PVector(mouseX, mouseY);
		path = new Path(start, end, 10);
		vehicle = new Vehicle(start.get());
		pathCreated = true;
	}
}

boolean pathCreated;
PVector start;

void draw(){
	background(255);
	if(pathCreated){
		vehicle.follow(path);
		vehicle.move();
		vehicle.render();
		path.render();
	}
	else if(start != null){
		ellipse(start.x, start.y, 10, 10);
	}
}

/**
 * Represents a line segment path.
 */
class Path {
	PVector start, end;
	float radius;

	Path(PVector start_, PVector end_, float radius_){
		start = start_;
		end = end_;
		radius = radius_;
	}

	void render(){
		strokeWeight(1);
		line(start.x, start.y, end.x, end.y);
		strokeWeight(1);
	}
}

/**
 * Represents an autonomous agent.
 */
class Vehicle {
	private PVector loc, vel, acc;
	private static final float MAX_FORCE = 0.1, MAX_SPEED = 3;

	Vehicle(PVector loc_){
		loc = loc_;
		vel = new PVector(MAX_SPEED, MAX_SPEED);
		acc = new PVector(0, 0);
	}

	/**
	 * `seek()` towards `path` if `this` Vehicle is on track to exit it.
	 */
	void follow(Path path){
		PVector projectedLoc = PVector.add(loc, vel);
		PVector a = PVector.sub(path.end, path.start),
			b = PVector.sub(projectedLoc, path.start);
		PVector pathVec = a.get();
		pathVec.normalize();
		pathVec.mult(a.dot(b) / a.mag());
		PVector normalEndpt = PVector.add(path.start, pathVec);

		float distFromLine = dist(
			projectedLoc.x, projectedLoc.y,
			normalEndpt.x, normalEndpt.y
		);
		if(distFromLine > path.radius){
			pathVec.normalize();
			pathVec.mult(20);
			normalEndpt.add(pathVec);
			seek(normalEndpt);
		}
	}

	/**
	 * Adjust acceleration to make this Vehicle's movement tend towards
	 * `targetLoc`.
	 */
	void seek(PVector targetLoc){
		PVector desired = PVector.sub(targetLoc, loc);
		if(desired.mag() == 0){
			return;
		}

		desired.normalize();
		desired.mult(MAX_SPEED);
		PVector steer = PVector.sub(desired, vel);
		steer.limit(MAX_FORCE);
		acc.add(steer);
	}

	/**
	 * Update `loc` according to `vel` and `acc`.
	 */
	void move(){
		vel.add(acc);
		vel.limit(MAX_SPEED);
		loc.add(vel);
		acc = new PVector(0, 0);
	}

	void render(){
		stroke(0);
		fill(175);
		pushMatrix();

		translate(loc.x, loc.y);
		rotate(vel.heading());
		triangle(10, 0, 0, 4, 0, -4);
		popMatrix();
	}
}
