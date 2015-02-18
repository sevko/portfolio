/**
 * A sketch containing an object that follows the user's mouse in a lifelike
 * manner.
 */

Vehicle vehicle;
void setup(){
	size(1000, 800);
	vehicle = new Vehicle(new PVector(width / 2, height / 2), 10);
}

void draw(){
	background(255);
	vehicle.seek(new PVector(mouseX, mouseY));
	vehicle.move();
	vehicle.render();
}

class Vehicle {
	private static final float MAX_SPEED = 8, MAX_FORCE = 1;
	private PVector loc, vel, acc;
	private float mass;

	Vehicle(PVector loc_, float mass_){
		this(loc_, new PVector(0, 0), mass_);
	}

	Vehicle(PVector loc_, PVector vel_, float mass_){
		loc = loc_;
		vel = vel_;
		acc = new PVector(0, 0);
		mass = mass_;
	}

	void seek(PVector targetLoc){
		PVector desiredVel = PVector.sub(targetLoc, loc);
		float targetDist = desiredVel.mag();
		desiredVel.normalize();
		if(targetDist < 100){
			desiredVel.mult((targetDist / 100) * MAX_SPEED);
		}
		else {
			desiredVel.mult(MAX_SPEED);
		}

		PVector steerForce = PVector.sub(desiredVel, vel);
		steerForce.limit(MAX_FORCE);

		acc.add(steerForce);
	}

	void move(){
		vel.add(acc);
		vel.limit(MAX_SPEED);
		loc.add(vel);
		acc.mult(0);
	}

	void render(){
		fill(175);
		pushMatrix();
		float x = loc.x, y = loc.y;
		translate(x, y);
		rotate(vel.heading());
		triangle(
			40, 0,
			0, -10,
			0, 10
		);
		popMatrix();
	}
}
