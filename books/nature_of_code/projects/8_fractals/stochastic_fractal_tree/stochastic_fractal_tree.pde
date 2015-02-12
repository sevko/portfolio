/**
 * Render a stochastic fractal tree.
 */

static final float branchAngle = PI / 8;

/**
 * Recursively renders a branch of the fractal tree, with a little bit of
 * entropy in the lengths/angles.
 */
void branch(int depth){
	if(--depth > 0){
		int branchLen = -(int)random(depth) * 10;
		float thetaRandomness = PI / 18;
		float theta = branchAngle + random(-thetaRandomness, thetaRandomness);
		line(0, 0, 0, branchLen);
		translate(0, branchLen);
		pushMatrix();
		rotate(theta);
		branch(depth);
		popMatrix();

		pushMatrix();
		rotate(-theta);
		branch(depth);
		popMatrix();
	}
}

void setup(){
	size(1000, 800);
	background(255);
	translate(width / 2, height / 2);
	branch(12);
}
