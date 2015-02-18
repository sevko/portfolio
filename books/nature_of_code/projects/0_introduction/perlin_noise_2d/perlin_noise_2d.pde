/**
 * A visualization of 2-dimensional Perlin noise.
 */

float perlinGradient, perlinX, perlinY;

void setup(){
	perlinGradient = 0.01;
	size(500, 500);
	loadPixels();
}

void draw(){
	perlinX = 0;
	for(int x = 0; x < width; x++){
		perlinY = 0;
		for(int y = 0; y < height; y++){
			float brightness = noise(perlinX, perlinY) * 255;
			pixels[x + y * width] = color(brightness);
			perlinY += perlinGradient;
		}
		perlinX += perlinGradient;
	}
	perlinGradient += 0.001;
	updatePixels();
}
