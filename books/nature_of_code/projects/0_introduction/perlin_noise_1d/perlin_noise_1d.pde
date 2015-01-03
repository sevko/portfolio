/**
 * A visualization of 1-dimensional Perlin noise.
 */

final float timeDelta = 0.01;
float time;

void setup(){
	time = 0;
	stroke(0);
	size(500, 500);
}

void draw(){
	float x = noise(time);
	time += timeDelta;

	float y = noise(time + 30);
	time += timeDelta;

	ellipse(x * width, y * height, 16, 16);
}
