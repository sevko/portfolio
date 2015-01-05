/**
 * An animation of simple harmonic motion, as exhibited by a circle moving
 * horizontally.
 */

final float PERIOD = 60,
	AMPLITUDE = 100;

void setup(){
	size(500, 500);
}

void draw(){
	background(255);
	float x = AMPLITUDE * cos(TWO_PI * frameCount / PERIOD);
	stroke(0);
	fill(175);
	translate(width / 2, height / 2);
	line(0, 0, x, 0);
	ellipse(x, 0, 20, 20);
}
