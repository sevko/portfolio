/**
 * A simulation of a weighted spring that can be manipulated with the user's
 * mouse. Click, drag, and release to set up a spring of a new length (counting
 * from the center of the window) with a weight at the cursor position.
 */

/**
 * A weighted spring.
 */
class Spring {
	Weight weight;

	final float SIZE = 3;
	PVector location;
	float length, k;

	Spring(float length_, float k_, PVector location_, Weight weight_){
		location = location_;
		length = length_;
		k = k_;
		weight = weight_;
	}

	void applyForceToWeight(){
		PVector distVec = PVector.sub(location, weight.location);
		float dist = distVec.mag();
		distVec.normalize();
		PVector force = PVector.mult(distVec, dist - length);
		weight.applyForce(force);
	}

	void update(PVector[] forces){
		applyForceToWeight();
		for(PVector force: forces){
			weight.applyForce(force);
		}
		weight.move();
		weight.acceleration = new PVector();
		weight.velocity.mult(0.98);
		render();
	}

	void render(){
		fill(0);
		ellipse(location.x, location.y, SIZE, SIZE);

		line(location.x, location.y, weight.location.x, weight.location.y);
		weight.render();
	}
}

class Weight {
	final float SIZE = 10;
	PVector location, velocity, acceleration;
	float mass;

	Weight(float mass_, PVector location_){
		velocity = new PVector();
		acceleration = new PVector();
		location = location_;
		mass = mass_;
	}

	void move(){
		velocity.add(acceleration);
		location.add(velocity);
	}

	void render(){
		noFill();
		ellipse(location.x, location.y, SIZE, SIZE);
	}

	void applyForce(PVector force){
		acceleration.add(PVector.div(force, mass));
	}
}

PVector offset;
PVector[] forces;
Spring spring;
float nextSpringLength;

void setup(){
	size(1000, 800);
	offset = new PVector(width / 2, height / 2);
	Weight weight = new Weight(10, new PVector(0, 220));
	spring = new Spring(200, 0.4, new PVector(0, 0), weight);
	forces = new PVector[] {
		new PVector(0, 9.8)
	};
}

/**
 * Record the selected length of the spring in `nextSpringLength`.
 */
void mousePressed(){
	nextSpringLength = dist(offset.x, offset.y, mouseX, mouseY);
}

/**
 * Create a spring of length `nextSpringLength`, and attach a weight to it
 * located at the current cursor position.
 */
void mouseReleased(){
	PVector weightLoc = new PVector(mouseX - offset.x, mouseY - offset.y);
	Weight weight = new Weight(10, weightLoc);
	spring = new Spring(nextSpringLength, 0.4, new PVector(0, 0), weight);
}

void draw(){
	background(255);
	translate(offset.x, offset.y);
	spring.update(forces);
}
