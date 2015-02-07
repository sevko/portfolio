/**
 * A sketch that renders particles moving through a flow field.
 */

PVector[][] vectorField;
public final static int SCREEN_WIDTH = 1000, SCREEN_HEIGHT = 700;
Particle[] particles;

void setup(){
	size(SCREEN_WIDTH, SCREEN_HEIGHT);
	particles = new Particle[400];
	for(int ind = 0; ind < particles.length; ind++){
		PVector loc = new PVector(random(0, width), random(0, height));
		particles[ind] = new Particle(loc);
	}
}

void draw(){
	background(255);
	for(Particle part : particles){
		part.acc = new PVector(
			(0.5 - noise(part.loc.x / 100, part.loc.heading())),
			(0.5 - noise(part.loc.y / 30, part.loc.heading()))
		);
		part.move();
		part.render();
	}
}

class Particle {
	private static final float MAX_SPEED = 3;
	PVector loc, vel, acc;

	Particle(PVector loc_){
		loc = loc_;
		vel = new PVector(0, 0);
		acc = new PVector(0, 0);
	}

	void move(){
		vel.add(acc);
		vel.limit(MAX_SPEED);
		loc.add(vel);
	}

	void render(){
		fill(175);
		pushMatrix();
		translate(loc.x, loc.y);
		rotate(vel.heading());
		triangle(8, 0, 0, 3, 0, -3);
		popMatrix();
	}
}
