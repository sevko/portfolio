/**
 * Render a bridge composed of circles bound together by `DistanceJoint`s.
 */

import shiffman.box2d.*;
import org.jbox2d.collision.shapes.*;
import org.jbox2d.common.*;
import org.jbox2d.dynamics.*;
import org.jbox2d.dynamics.joints.DistanceJointDef;
import org.jbox2d.dynamics.joints.DistanceJoint;

class Component {
	private static final int BODY_RADIUS = 5;
	Body body;

	Component(float x, float y){
		BodyDef bodyDef = new BodyDef();
		bodyDef.type = BodyType.DYNAMIC;
		bodyDef.position.set(box2d.coordPixelsToWorld(x, y));
		body = box2d.createBody(bodyDef);

		CircleShape shape = new CircleShape();
		shape.m_radius = box2d.scalarPixelsToWorld(BODY_RADIUS);
		body.createFixture(shape, 1);
	}

	void display(){
		fill(175);
		Vec2 pos = box2d.getBodyPixelCoord(body);
		ellipse(pos.x, pos.y, BODY_RADIUS, BODY_RADIUS);
	}
}

Box2DProcessing box2d;
Component[] components;

void setup(){
	size(1000, 800);
	box2d = new Box2DProcessing(this);
	box2d.createWorld();

	int numComponents = 40;
	components = new Component[numComponents];

	float jointLength = width / components.length;
	for(
		int ind = 0, xOffset = 0;
		ind < components.length;
		ind++, xOffset += jointLength
	){
		components[ind] = new Component(xOffset, 300);
	}

	for(int ind = 0; ind < components.length - 1; ind++){
		DistanceJointDef jointDef = new DistanceJointDef();
		jointDef.bodyA = components[ind].body;
		jointDef.bodyB = components[ind + 1].body;
		jointDef.length = box2d.scalarPixelsToWorld(jointLength);
		jointDef.frequencyHz = 0;
		jointDef.dampingRatio = 0;
		box2d.world.createJoint(jointDef);
	}

	for(int ind: new int[]{0, components.length - 1}){
		components[ind].body.setType(BodyType.STATIC);
	}
}

void draw(){
	background(255);
	box2d.step();
	for(Component comp: components){
		comp.display();
	}
}
