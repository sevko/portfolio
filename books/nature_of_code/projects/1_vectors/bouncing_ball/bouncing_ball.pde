/**
 *
 */

float x = 100;
float y = 100;
float dx = 1;
float dy = 3.3;

void setup(){
	size(640, 360);
	background(255);
}

void draw(){
	background(255);

	x += dx;
	y += dy;

	if((x > width) || (x < 0)){
		dx *= -1;
	}

	if((y > height) || (y < 0)){
		dy *= -1;
	}

	stroke(0);
	fill(175);
	ellipse(x, y, 16, 16);
}
