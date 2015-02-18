/**
 * A sketch demonstrating mutual repulsion between a swarm of actors.
 */

Vehicle[] vehicles;

void setup(){
	size(1000, 800);
	vehicles = new Vehicle[300];
	for(int ind = 0; ind < vehicles.length; ind++){
		vehicles[ind] = new Vehicle(
			new PVector(random(width), random(height)),
			new PVector(random(-2, 2), random(-2, 2)),
			20
		);
	}
}

void draw(){
	background(255);
	for(Vehicle vehicle: vehicles){
		vehicle.repel(vehicles);
	}

	for(Vehicle vehicle: vehicles){
		vehicle.move();
		vehicle.render();
	}
}

/**
 * A mobile actor that implements repulsion from other actors.
 */
class Vehicle {
	private static final float MAX_SPEED = 3, MAX_FORCE = 1;
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

	/**
	 * Adjust this Vehicle's `acc` to repel it from the nearby Vehicles in
	 * `vehicles`.
	 */
	void repel(Vehicle[] vehicles){
		PVector repulsion = new PVector(0, 0);
		int numRepellers = 0;

		for(Vehicle vehicle : vehicles){
			float dist = dist(vehicle.loc.x, vehicle.loc.y, loc.x, loc.y);
			float desiredMinDist = 5 * mass;
			if(0 < dist && dist < desiredMinDist){
				PVector displacement = PVector.sub(loc, vehicle.loc);
				displacement.normalize();
				displacement.div(dist);
				repulsion.add(displacement);
				numRepellers++;
			}
		}

		if(numRepellers > 0){
			acc.add(repulsion);
		}
	}

	void move(){
		vel.add(acc);
		vel.limit(MAX_SPEED);
		loc.add(vel);

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

	void render(){
		fill(175);
		pushMatrix();
		float x = loc.x, y = loc.y;
		translate(x, y);
		rotate(vel.heading());
		triangle(
			8, 0,
			0, -3,
			0, 3
		);
		popMatrix();
	}
}
