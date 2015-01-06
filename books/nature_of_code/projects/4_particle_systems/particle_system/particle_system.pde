/**
 * A particle system simulation.
 */

/**
 * A particle with a location, velocity, acceleration, angle, and angular
 * acceleration, that expires over time.
 */
class Particle {
	PVector location, velocity, acceleration;
	float angle, angAcceleration;
	int life;

	Particle(PVector location_){
		location = location_;
		velocity = new PVector(3, -3);
		acceleration = new PVector(0, 0.1);

		life = 255;
		angle = 0;
		angAcceleration = radians(5);
	}

	void move(){
		angle += angAcceleration;
		velocity.add(acceleration);
		location.add(velocity);
	}

	boolean isExpired(){
		return life <= 0;
	}

	void render(){
		noStroke();
		fill(0, life);

		pushMatrix();
		translate(location.x, location.y);
		rotate(angle);
		rectMode(CENTER);
		rect(0, 0, 10, 40);
		popMatrix();
	}

	void update(){
		move();
		render();
		life--;
	}
}

Particle particle;

void setup(){
	size(1000, 700);
	particle = new Particle(new PVector(width / 2, height / 2));
}

void draw(){
	background(255);
	particle.update();
	if(particle.isExpired()){
		println("Expired.");
		noLoop();
	}
}
