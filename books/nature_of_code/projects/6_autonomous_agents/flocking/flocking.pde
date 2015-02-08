/**
 *
 */

void setup(){
}

void draw(){
}

/**
 * An actor that implements flock behavior.
 */
class Boid {
	private static final float MAX_FORCE = 3,
		MAX_SPEED = 0.05,
		radius = 3;
	PVector loc, vel, acc;

	Boid(PVector loc_){
		loc = loc_;
		vel = new PVector(0, 0);
		acc = new PVector(0, 0);
	}

	/**
	 * Render this Boid on the canvas.
	 */
	void render(){
		fill(175);
		pushMatrix();
		translate(loc.x, loc.y);
		rotate(vel.heading());
		triangle(8, 0, 0, 3, 0, -3);
		popMatrix();
	}

	/**
	 * Move this Boid, updating its acc, vel, and loc.
	 */
	void move(){
		acc.limit(MAX_FORCE);
		vel.add(acc);
		vel.limit(MAX_SPEED);
		loc.add(vel);
		acc.mult(0);
	}

	void applyForce(PVector force){
		acc.add(force);
	}

	void flock(ArrayList<Boid> boids){
		applyForce(separate(boids));
		applyForce(align(boids));
		applyForce(cohesion(boids));
	}

	private PVector separate(ArrayList<Boid> boids){
	}

	private PVector align(ArrayList<Boid> boids){
	}

	private PVector cohesion(ArrayList<Boid> boids){
	}
}
