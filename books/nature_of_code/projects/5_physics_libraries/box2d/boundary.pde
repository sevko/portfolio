class Boundary implements Renderable {
	float x, y;
	float width_, height_;

	Boundary(float x_, float y_, float widthA, float heightA){
		x = x_;
		y = y_;
		width_ = widthA;
		height_ = heightA;

		BodyDef bodyDef = new BodyDef();
		bodyDef.position.set(box2d.coordPixelsToWorld(x, y));
		bodyDef.type = BodyType.STATIC;
		Body body = box2d.createBody(bodyDef);

		float boxWidth = box2d.scalarPixelsToWorld(widthA / 2);
		float boxHeight = box2d.scalarPixelsToWorld(heightA / 2);
		PolygonShape shape = new PolygonShape();
		shape.setAsBox(boxWidth, boxHeight);
		body.createFixture(shape, 1);
	}

	void display(){
		fill(0);
		stroke(0);
		rectMode(CENTER);
		rect(x, y, width_, height_);
	}
}

class CurvedBoundary implements Renderable {
	Vec2[] vertices;

	CurvedBoundary(Vec2[] vertices_){
		vertices = vertices_;
		Body body = box2d.world.createBody(new BodyDef());
		ChainShape shape = new ChainShape();

		Vec2[] worldVertices = new Vec2[vertices.length];
		for(int ind = 0; ind < vertices.length; ind++){
			worldVertices[ind] = box2d.coordPixelsToWorld(vertices[ind]);
		}
		shape.createChain(worldVertices, worldVertices.length);

		FixtureDef fixtureDef = new FixtureDef();
		fixtureDef.shape = shape;
		fixtureDef.density = 1;
		fixtureDef.friction = 0.3;
		fixtureDef.restitution = 0.5;
		body.createFixture(fixtureDef);
	}

	void display(){
		strokeWeight(1);
		stroke(0);
		noFill();
		beginShape();
		for(Vec2 vert: vertices){
			vertex(vert.x, vert.y);
		}
		endShape();
	}
}
