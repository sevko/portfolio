/**
 * An experiment with Box2d forces, which consists of a number of freefalling
 * particles that will accelerate towards the location of the user's mouse when
 * pressed.
 */

import shiffman.box2d.*;
import org.jbox2d.collision.shapes.*;
import org.jbox2d.common.*;
import org.jbox2d.dynamics.*;

class Particle {
	private static final float BODY_RADIUS = 10;

	Body body;

	Particle(float x, float y){
		BodyDef bodyDef = new BodyDef();
		bodyDef.type = BodyType.DYNAMIC;
		bodyDef.position.set(box2d.coordPixelsToWorld(x, y));
		body = box2d.createBody(bodyDef);

		CircleShape shape = new CircleShape();
		shape.m_radius = box2d.scalarPixelsToWorld(BODY_RADIUS);
		body.createFixture(shape, 1);
	}

	void display(){
		stroke(0);
		fill(175);

		Vec2 pos = box2d.getBodyPixelCoord(body);
		float angle = body.getAngle();

		pushMatrix();
		translate(pos.x, pos.y);
		rotate(angle);
		ellipse(0, 0, BODY_RADIUS, BODY_RADIUS);
		popMatrix();
	}
}

Box2DProcessing box2d;
Particle[] particles  = new Particle[20];

void setup(){
	size(1000, 300);
	box2d = new Box2DProcessing(this);
	box2d.createWorld();

	for(int ind = 0; ind < particles.length; ind++){
		particles[ind] = new Particle(random(0, width), random(0, height));
	}
}

void mousePressed(){
	for(Particle particle : particles){
		Vec2 mousePos = box2d.coordPixelsToWorld(new Vec2(mouseX, mouseY));
		Vec2 partPos = particle.body.getWorldCenter();
		Vec2 force = mousePos.sub(partPos);
		force.normalize();
		force.mulLocal(4000);
		particle.body.applyForce(force, partPos);
	}
}

void draw(){
	background(255);
	box2d.step();
	for(Particle particle : particles){
		particle.display();
	}
}
