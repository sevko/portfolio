/**
 * Create simple `Box` objects wherever the user clicks, and subject them to
 * physics with Box2D.
 */

import shiffman.box2d.*;
import org.jbox2d.collision.shapes.*;
import org.jbox2d.common.*;
import org.jbox2d.dynamics.*;

Box2DProcessing box2d;
ArrayList<Renderable> objects;

void setup(){
	size(1000, 300);
	box2d = new Box2DProcessing(this);
	box2d.createWorld();

	objects = new ArrayList<Renderable>();
	objects.add(new Boundary(40, 40, 100, 10));
	objects.add(new Boundary(200, 300, 120, 10));

	Vec2[] vertices = new Vec2[100 + 2];
	int x;
	for(x = 0; x < vertices.length - 2; x++){
		vertices[x] = new Vec2(
			x * (width / vertices.length),
			height - noise(x * 0.1) * 50
		);
	}
	vertices[x++] = new Vec2(width, height);
	vertices[x] = new Vec2(0, height);
	objects.add(new CurvedBoundary(vertices));
}

void mousePressed(){
	Pair pair = new Pair(
		new Vec2(mouseX - 5, mouseY),
		new Vec2(mouseX + 5, mouseY)
	);
	objects.add(pair);
}

void draw(){
	box2d.step();
	background(255);
	for(Renderable obj: objects){
		obj.display();
	}
}

interface Renderable {
	void display();
}
