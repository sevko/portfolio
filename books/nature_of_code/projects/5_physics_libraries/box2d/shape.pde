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
