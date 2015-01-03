/**
 * A simulation of a moving object that accelerates towards the mouse.
 */

class Mover {
	final float SIZE = 30;

	PVector location, velocity, acceleration;

	Mover(){
		location = new PVector(width / 2, height / 2);
		velocity = new PVector(0, 0);
	}

	/**
	 * Accelerate towards the mouse. Note that the mover's location wraps
	 * around the screen.
	 */
	void move(){
		acceleration = new PVector(mouseX, mouseY);
		acceleration.sub(location);
		acceleration.normalize();
		acceleration.div(20);
		velocity.add(acceleration);
		velocity.limit(5);
		location.add(velocity);

		if(location.x < 0){
			location.x = height;
		}
		else if(location.x > height){
			location.x = 0;
		}

		if(location.y < 0){
			location.y = width;
		}
		else if(location.y > width){
			location.y = 0;
		}
	}

	void display(){
		stroke(0);
		fill(175);
		ellipse(location.x, location.y, SIZE, SIZE);
	}
}

Mover mover;

void setup(){
	size(500, 500);
	mover = new Mover();
}

void draw(){
	background(255);
	mover.move();
	mover.display();
}
