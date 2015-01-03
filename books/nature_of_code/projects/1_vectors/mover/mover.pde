/**
 *
 */

class PVector {
	float x, y;

	PVector(float x_, float y_){
		x = x_;
		y = y_;
	}

	void add(PVector vec){
		x += vec.x;
		y += vec.y;
	}

	void limit(float val){
		if(x > val){
			x = val;
		}

		if(y > val){
			y = val;
		}
	}
}

class Mover {
	PVector location, velocity, acceleration;

	Mover(){
		location = new PVector(width / 2, height / 2);
		velocity = new PVector(0, 0);
		acceleration = new PVector(-0.001, 0.01);
	}

	void move(){
		velocity.add(acceleration);
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
		ellipse(location.x, location.y, 16, 16);
	}
}

Mover mover;

void setup(){
	size(300, 300);
	mover = new Mover();
}

void draw(){
	background(255);
	mover.move();
	mover.display();
}
