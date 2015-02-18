/**
 * A sketch demonstrating two particles connected by a spring, using toxiclibs
 * to simulate physics.
 */

import toxi.physics2d.*;
import toxi.physics2d.behaviors.GravityBehavior;
import toxi.geom.*;

VerletPhysics2D physics;
Particle p1, p2;

void setup(){
	size(800, 360);
	physics = new VerletPhysics2D();
	physics.addBehavior(new GravityBehavior(new Vec2D(0, 0.5)));
	physics.setWorldBounds(new Rect(0, 0, width, height));

	p1 = new Particle(new Vec2D(100, 20));
	p2 = new Particle(new Vec2D(100, 180));

	p1.lock();
	VerletSpring2D spring = new VerletSpring2D(p1, p2, 80, 0.01);
	physics.addParticle(p1);
	physics.addParticle(p2);
	physics.addSpring(spring);
}

void draw(){
	physics.update();
	background(255);
	line(p1.x, p1.y, p2.x, p2.y);
	p1.display();
	p2.display();

	if(mousePressed){
		p2.lock();
		p2.x = mouseX;
		p2.y = mouseY;
		p2.unlock();
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
