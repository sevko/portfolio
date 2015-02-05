/**
 * A simple object that maintains a `Box2D` state.
 */

// import org.jbox2d.dynamics.joints.DistanceJointDef;
// import org.jbox2d.dynamics.joints.DistanceJoint;
// import org.jbox2d.dynamics.joints.RevoluteJointDef;
import org.jbox2d.dynamics.joints.*;

class Box implements Renderable {
	float height_, width_;
	Body body;

	Box(float x, float y, float widthArg, float heightArg, boolean fixed){
		width_ = widthArg;
		height_ = heightArg;

		BodyDef bodyDef = new BodyDef();
		bodyDef.type = fixed ? BodyType.STATIC : BodyType.DYNAMIC;
		bodyDef.position.set(box2d.coordPixelsToWorld(x, y));
		body = box2d.createBody(bodyDef);

		PolygonShape shape = new PolygonShape();
		float worldWidth = box2d.scalarPixelsToWorld(width_ / 2);
		float worldHeight = box2d.scalarPixelsToWorld(height_ / 2);
		shape.setAsBox(worldWidth, worldHeight);

		body.createFixture(shape, 1);
	}

	void display(){
		Vec2 pos = box2d.getBodyPixelCoord(body);
		float angle = body.getAngle();

		pushMatrix();
		translate(pos.x, pos.y);
		rotate(-angle);
		fill(175);
		stroke(0);
		rectMode(CENTER);
		rect(0, 0, width_, height_);
		popMatrix();
	}

	void destroy(){
		box2d.destroyBody(body);
	}
}

class Polygon implements Renderable {
	Body body;

	Polygon(float x, float y, Vec2[] vertices){
		BodyDef bodyDef = new BodyDef();
		bodyDef.type = BodyType.DYNAMIC;
		bodyDef.position.set(box2d.coordPixelsToWorld(x, y));
		body = box2d.createBody(bodyDef);

		PolygonShape shape = new PolygonShape();
		Vec2[] worldVerts = new Vec2[vertices.length];
		for(int ind = 0; ind < vertices.length; ind++){
			worldVerts[ind] = box2d.vectorPixelsToWorld(vertices[ind]);
		}
		shape.set(worldVerts, worldVerts.length);
		body.createFixture(shape, 1);
	}

	void display(){
		Fixture fixture = body.getFixtureList();
		PolygonShape shape = (PolygonShape) fixture.getShape();

		rectMode(CENTER);
		fill(175);
		stroke(0);
		pushMatrix();

		Vec2 pos = box2d.getBodyPixelCoord(body);
		float angle = body.getAngle();
		translate(pos.x, pos.y);
		rotate(-angle);

		beginShape();
		for(int ind = 0; ind < shape.getVertexCount(); ind++){
			Vec2 vert = box2d.vectorWorldToPixels(shape.getVertex(ind));
			vertex(vert.x, vert.y);
		}
		endShape(CLOSE);
		popMatrix();
	}
}

/**
 * A figure with a rectangular body and circular head.
 */
class Figure implements Renderable {
	private final static int BODY_WIDTH = 10,
		BODY_HEIGHT = 30,
		HEAD_RADIUS = 5;

	Body body;

	Figure(float x, float y){
		BodyDef bodyDef = new BodyDef();
		bodyDef.type = BodyType.DYNAMIC;
		bodyDef.position.set(box2d.coordPixelsToWorld(x, y));
		body = box2d.createBody(bodyDef);

		PolygonShape bodyShape = new PolygonShape();
		bodyShape.setAsBox(
			box2d.scalarPixelsToWorld(BODY_WIDTH / 2),
			box2d.scalarPixelsToWorld(BODY_HEIGHT / 2)
		);
		body.createFixture(bodyShape, 1);

		CircleShape headShape = new CircleShape();
		headShape.m_radius = box2d.scalarPixelsToWorld(HEAD_RADIUS);
		Vec2 worldHeadOffset = box2d.vectorPixelsToWorld(
			new Vec2(0, -BODY_HEIGHT / 2)
		);
		headShape.m_p.set(worldHeadOffset.x, worldHeadOffset.y);
		body.createFixture(headShape, 1);
	}

	void display(){
		rectMode(CENTER);
		fill(175);
		pushMatrix();

		Vec2 pos = box2d.getBodyPixelCoord(body);
		float angle = body.getAngle();
		translate(pos.x, pos.y);
		rotate(-angle);

		rect(0, 0, BODY_WIDTH, BODY_HEIGHT);
		ellipse(0, -BODY_HEIGHT / 2, HEAD_RADIUS * 2, HEAD_RADIUS * 2);
		popMatrix();
	}
}

class Pair implements Renderable {
	private class Particle {
		Body body;

		Particle(Vec2 pos){
			BodyDef bodyDef = new BodyDef();
			bodyDef.type = BodyType.DYNAMIC;
			bodyDef.position.set(box2d.coordPixelsToWorld(pos.x, pos.y));
			body = box2d.createBody(bodyDef);

			CircleShape shape = new CircleShape();
			shape.m_radius = box2d.scalarPixelsToWorld(5);
			body.createFixture(shape, 1);
		}

		void display(){
			pushMatrix();

			Vec2 pos = box2d.getBodyPixelCoord(body);
			float angle = body.getAngle();
			translate(pos.x, pos.y);
			rotate(-angle);

			Fixture fixture = body.getFixtureList();
			CircleShape shape = (CircleShape) fixture.getShape();
			float radius = box2d.scalarWorldToPixels(shape.getRadius());
			ellipse(0, 0, radius, radius);

			popMatrix();
		}
	}

	private Particle p1, p2;
	private final static int LENGTH = 32;

	Pair(Vec2 pos1, Vec2 pos2){
		p1 = new Particle(pos1);
		p2 = new Particle(pos2);
		DistanceJointDef jointDef = new DistanceJointDef();
		jointDef.bodyA = p1.body;
		jointDef.bodyB = p2.body;
		jointDef.length = box2d.scalarPixelsToWorld(LENGTH);
		jointDef.frequencyHz = 0;
		jointDef.dampingRatio = 0;
		box2d.world.createJoint(jointDef);
	}

	void display(){
		Vec2 pos1 = box2d.getBodyPixelCoord(p1.body);
		Vec2 pos2 = box2d.getBodyPixelCoord(p2.body);
		stroke(0);

		line(pos1.x, pos1.y, pos2.x, pos2.y);
		p1.display();
		p2.display();
	}
}

/**
 * A windmill with a self-propelled rotor.
 */
class Windmill implements Renderable {
	RevoluteJoint joint;
	Box box1, box2;

	Windmill(float x, float y){
		box1 = new Box(x, y, 120, 10, false);
		box2 = new Box(x, y, 10, 40, true);

		RevoluteJointDef jointDef = new RevoluteJointDef();
		jointDef.initialize(box1.body, box2.body, box1.body.getWorldCenter());
		jointDef.motorSpeed = PI * 2;
		jointDef.maxMotorTorque = 1e3;
		jointDef.enableMotor = true;
		joint = (RevoluteJoint) box2d.world.createJoint(jointDef);
	}

	void toggleMotor(){
		joint.enableMotor(!joint.isMotorEnabled());
	}

	void display(){
		box1.display();
		box2.display();
	}
}
