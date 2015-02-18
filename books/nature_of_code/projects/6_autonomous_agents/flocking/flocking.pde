/**
 * A sketch demonstrating flocking behavior.
 */

Boid[] boids;

void setup(){
	size(1000, 800);
	boids = new Boid[300];
	for(int ind = 0; ind < boids.length; ind++){
		boids[ind] = new Boid(
			new PVector(random(width), random(height)),
			new PVector(random(-4, 4), random(-4, 4))
		);
	}
}

void draw(){
	background(255);
	for(Boid boid : boids){
		boid.flock(boids);
	}

	for(Boid boid : boids){
		boid.move();
		boid.render();
	}
}

/**
 * An actor that implements flocking behavior.
 */
class Boid {
	private static final float MAX_FORCE = 0.05,
		MAX_SPEED = 10,
		radius = 3;
	PVector loc, vel, acc;

	Boid(PVector loc_, PVector vel_){
		loc = loc_;
		vel = vel_;
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

		// Wrap `loc` around the canvas's borders.
		if(loc.x > width){
			loc.x = 0;
		}
		else if(loc.x < 0){
			loc.x = width;
		}

		if(loc.y > height){
			loc.y = 0;
		}
		else if(loc.y < 0){
			loc.y = height;
		}

		acc.mult(0);
	}

	void applyForce(PVector force){
		acc.add(force);
	}

	void flock(Boid[] boids){
		float neighborDist = 30;
		ArrayList<Boid> neighbors = new ArrayList<Boid>();
		for(Boid boid : boids){
			float distance = dist(boid.loc.x, boid.loc.y, loc.x, loc.y);
			if(0 < distance && distance < neighborDist){
				neighbors.add(boid);
			}
		}

		if(neighbors.size() > 0){
			applyForce(separate(neighbors));
			applyForce(align(neighbors));
			applyForce(cohesion(neighbors));
		}
	}

	/**
	 * Apply a force to this Boid to separate it from all the Boids in `boids`.
	 */
	private PVector separate(ArrayList<Boid> boids){
		PVector separation = new PVector(0, 0);

		for(Boid boid : boids){
			PVector disp = PVector.sub(loc, boid.loc);
			disp.normalize();
			separation.add(disp);
		}

		separation.div(boids.size());
		separation.normalize();
		separation.mult(MAX_SPEED);
		PVector steer = PVector.sub(separation, vel);
		return steer;
	}

	/**
	 * Apply a force to this Boid to align its movement with that of the Boids
	 * in `boids`.
	 */
	private PVector align(ArrayList<Boid> boids){
		PVector alignment = new PVector(0, 0);

		for(Boid boid : boids){
			alignment.add(boid.vel);
		}

		alignment.div(boids.size());
		alignment.normalize();
		alignment.mult(MAX_SPEED);
		PVector steer = PVector.sub(alignment, vel);
		return steer;
	}

	/**
	 * Apply a force to this Boid to move it closer to the Boids in `boids`.
	 */
	private PVector cohesion(ArrayList<Boid> boids){
		PVector cohesion = new PVector(0, 0);

		for(Boid boid : boids){
			cohesion.add(boid.loc);
		}

		cohesion.div(boids.size());
		return cohesion;
	}
}
