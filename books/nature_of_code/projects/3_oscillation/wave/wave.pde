/**
 * An animation of a sinusoidal wave increasing in amplitude (to a reasonable
 * upper bound).
 */

final int NUM_POINTS = 40, POINT_SIZE = 5;

void setup(){
	size(1000, 500);
}

void draw(){
	background(255);
	translate(0, height / 2);

	float amplitude = constrain(frameCount / 2, 1, height / 3);

	float xDelta = width / (NUM_POINTS - 1);

	for(int ind = 0; ind < NUM_POINTS; ind++){
		float x = xDelta * ind + POINT_SIZE;
		float y = amplitude * sin((ind + frameCount) * TWO_PI / NUM_POINTS);
		ellipse(x, y, POINT_SIZE, POINT_SIZE);
	}
}
