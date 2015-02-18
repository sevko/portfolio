/**
 * Creates a walker that draws a biased random path around the screen.
 */

import java.util.Random;


class Walker {
	float x, y;
	Random distributionGen;

	Walker(){
		x = width / 2;
		y = height / 2;
		distributionGen = new Random();
	}

	void display(){
		stroke(0);
		fill(10);
		ellipse(x, y, 20, 20);
	}

	/**
	 * Move a random amount in the `x` and `y` directions.
	 */
	void step(){
		float choice = random(1);
		// float distance = abs((float)distributionGen.nextGaussian());
		float distance = monteCarlo();

		if(choice < 0.25){
			x += distance;
		}
		else if(choice < 0.5){
			x += distance;
		}
		else if(choice < 0.8){
			y += distance;
		}
		else {
			y -= distance;
		}
	}
}

float monteCarlo(){
	float value, chosen;
	do {
		value = random(1);
		chosen = random(1);
	} while(chosen >= (value * value));
	return value;
}

Walker walker;

void setup(){
	size(640, 360);
	walker = new Walker();
	background(255);
}

void draw(){
	walker.step();
	walker.display();
}
