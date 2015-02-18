/**
 * Renders a normal distribution of colored circles.
 */

import java.util.Random;

Random generator;

final float stdDeviation = 60,
	mean = 320;

void setup(){
	size(640, 360);
	generator = new Random();
}

void draw(){
	float num = (float) generator.nextGaussian();
	float pos = num * stdDeviation + mean;
	noStroke();
	fill(255, 10);
	ellipse(pos, 180, 16, 16);
}
