/**
 * Render the Cantor Set to 14 iterations.
 */

void cantorSet(int x1, int x2, int y, int numIterations){
	if(numIterations-- > 0){
		line(x1, y, x2, y);
		int thirdDist = (x2 - x1) / 3;
		cantorSet(x1, x1 + thirdDist, y + 20, numIterations);
		cantorSet(x2 - thirdDist, x2, y + 20, numIterations);
	}
}

void setup(){
	size(1000, 800);
	background(255);
	cantorSet(
		(int)(width * 0.1), (int)(width * 0.9), (int)(height * 0.1),
		14
	);
}
