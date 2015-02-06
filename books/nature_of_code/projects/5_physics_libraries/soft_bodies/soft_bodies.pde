/**
 * A pendulum simulated with toxiclibs.
 */

import toxi.physics2d.*;
import toxi.physics2d.behaviors.GravityBehavior;
import toxi.geom.*;

final int NUM_PARTICLES = 20;
Particle[] particles  = new Particle[NUM_PARTICLES];
VerletPhysics2D physics;

void setup(){
	size(1000, 500);
	physics = new VerletPhysics2D();
	physics.addBehavior(new GravityBehavior(new Vec2D(0, 0.5)));

	float xSpacing = 20;
	for(int ind = 0; ind < NUM_PARTICLES; ind++){
		Vec2D loc = new Vec2D(width / 2 + xSpacing * ind, height / 2);
		Particle part = new Particle(loc);
		physics.addParticle(part);
		particles[ind] = part;

		if(ind > 0){
			VerletSpring2D spring = new VerletSpring2D(
				part, particles[ind - 1], xSpacing, 0.1
			);
			physics.addSpring(spring);
		}
		else {
			part.lock();
		}
	}
}

void draw(){
	physics.update();
	background(255);
	for(Particle part : particles){
		part.display();
	}
}

class Particle extends VerletParticle2D {
	Particle(Vec2D loc){
		super(loc);
	}

	void display(){
		fill(175);
		stroke(0);
		ellipse(x, y, 16, 16);
	}
}
