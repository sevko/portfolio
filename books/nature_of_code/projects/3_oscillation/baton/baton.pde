/**
 * Render a twirling baton experiencing constant angular acceleration.
 */

float angle = 0;
float angVelocity = 0;
float angAcceleration = 0.001;

void setup(){
	size(1000, 800);
}

void draw(){
	background(255);

	fill(175);
	stroke(0);
	rectMode(CENTER);

	translate(width / 2, height / 2);
	rotate(angle);

	line(-50, 0, 50, 0);
	ellipse(50, 0, 8, 8);
	ellipse(-50, 0, 8, 8);

	angVelocity += angAcceleration;
	angle += angVelocity;
}
