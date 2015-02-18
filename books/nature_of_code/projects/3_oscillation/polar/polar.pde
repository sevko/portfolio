/**
 * An animation of a circle travelling along the graph of the polar equation
 * `r = theta`.
 */

float theta;
final float THETA_DELTA = 0.3;

void setup(){
	size(400, 400);
	theta = 0;
}

void draw(){
	background(255);
	theta += THETA_DELTA;
	translate(width / 2, height / 2);
	float x = cos(theta) * theta;
	float y = sin(theta) * theta;
	line(0, 0, x, y);
	ellipse(x, y, 10, 10);
}
