/**
 * Create simple `Box` objects wherever the user clicks, and subject them to
 * physics with Box2D.
 */

import shiffman.box2d.*;
import org.jbox2d.collision.shapes.*;
import org.jbox2d.common.*;
import org.jbox2d.dynamics.*;

Box2DProcessing box2d;
ArrayList<Box> boxes;

void setup(){
	size(400, 300);
	boxes = new ArrayList<Box>();
	box2d = new Box2DProcessing(this);
	box2d.createWorld();
}

void mousePressed(){
	boxes.add(new Box(mouseX, mouseY));
}

void draw(){
	box2d.step();
	background(255);
	for(Box box: boxes){
		box.display();
	}
}
