/**
 * A simple simulation involving multiple actors influencing one another with
 * gravity.
 */

/**
 * An actor with mass and motion.
 */
class Attractor {
	PVector location, velocity, acceleration;
	float mass;
	int size;

	Attractor(float mass_){
		location = new PVector(random(width), random(height));
		velocity = new PVector();
		acceleration = new PVector();
		mass = mass_;
		size = (mass_ > 50) ? 100 : (int)mass_;
	}

	void move(){
		velocity.add(acceleration);
		location.add(velocity);
	}

	void display(){
		stroke(0);
		noFill();
		ellipse(location.x, location.y, size, size);
	}

	/**
	 * Apply a force to this Attractor, updating its acceleration.
	 */
	void applyForce(PVector force){
		acceleration.add(PVector.div(force, mass));
	}

	/**
	 * Compute the gravitational force exerted on `attr` by this Attractor.
	 */
	PVector getGravity(Attractor attr){
		PVector disp = PVector.sub(location, attr.location);
		float dist = constrain(size, disp.mag(), 300);
		float gravity = mass * attr.mass / sq(dist);
		disp.normalize();
		return PVector.mult(disp, gravity);
	}
}

Attractor[] attractors;
final int NUM_ATTRACTORS = 100;

void setup(){
	size(1000, 800);
	attractors = new Attractor[NUM_ATTRACTORS];
	for(int attr = 0; attr < attractors.length - 1; attr++){
		attractors[attr] = new Attractor(random(1, 20));
	}

	attractors[attractors.length - 1] = new Attractor(400);
}

/**
 * Compute the gravity exerted on any one `Attractor` in `attractors` by all of
 * the others, apply that force, then move and render them all.
 */
void draw(){
	background(255);
	for(int attr1 = 0; attr1 < attractors.length; attr1++){
		Attractor targetAttr = attractors[attr1];
		for(int attr2 = 0; attr2 < attractors.length; attr2++){
			if(attr2 != attr1){
				PVector gravity = attractors[attr2].getGravity(targetAttr);
				targetAttr.applyForce(gravity);
			}
		}
	}

	for(Attractor attr: attractors){
		attr.move();
		attr.display();
		attr.acceleration = new PVector();
	}
}
