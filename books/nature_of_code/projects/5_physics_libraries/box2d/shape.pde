/**
 * A simple object that maintains a `Box2D` state.
 */

class Box implements Renderable {
	float height_, width_;
	Body body;

	Box(float x, float y){
		height_ = 10;
		width_ = 16;

		BodyDef bodyDef = new BodyDef();
		bodyDef.type = BodyType.DYNAMIC;
		bodyDef.position.set(box2d.coordPixelsToWorld(x, y));
		body = box2d.createBody(bodyDef);

		PolygonShape shape = new PolygonShape();
		float worldWidth = box2d.scalarPixelsToWorld(width_ / 2);
		float worldHeight = box2d.scalarPixelsToWorld(height_ / 2);
		shape.setAsBox(worldWidth, worldHeight);

		FixtureDef fixture = new FixtureDef();
		fixture.shape = shape;
		fixture.density = 1;
		fixture.friction = 0.3;
		fixture.restitution = 0.5;
		body.createFixture(fixture);
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
