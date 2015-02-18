/**
 * Renders a splattered-paint image using a normal distribution.
 */

import java.util.Random;

final float mean = 200,
	stdDeviation = 30;
Random distributionGen;

void setup(){
	size(400, 400);
	distributionGen = new Random();
}

void draw(){
	float xSeed = (float)distributionGen.nextGaussian(),
		ySeed = (float)distributionGen.nextGaussian();

	float x = xSeed * stdDeviation + mean;
	float y = ySeed * stdDeviation + mean;
	int pointColor = (int)((abs(xSeed) + abs(ySeed)) / 2 * 0xff);

	stroke(0xff, pointColor, pointColor);
	fill(0xff, pointColor, pointColor);
	ellipse(x, y, 3, 3);
}
