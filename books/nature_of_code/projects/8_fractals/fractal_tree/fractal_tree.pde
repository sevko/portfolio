/**
 * Render a fractal tree.
 */

static final float branchAngle = PI / 8;

/**
 * Recursively renders a branch of the fractal tree.
 */
void branch(int depth){
	if(--depth > 0){
		int branchLen = -4 * depth;
		float theta = branchAngle - radians(8 / depth);
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
	translate(width / 2, height / 2);
	branch(12);
}
