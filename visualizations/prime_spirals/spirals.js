/**
 * Render an Ulam spiral on the page's canvas.
 *
 * @param {number} numLayers The number of concentric rings, or layers, in the
 *      spiral.
 */
function ulamSpiral(numLayers){
	var drawPixel = setupCanvas(numLayers);

	var currValue = 1;
	var x = 0;
	var y = 0;

	for(var layer = 0, len = 0; layer <= numLayers; layer++, len += 2){
		function drawLine(dx, dy, len){
			for(var pixel = 0; pixel < len; pixel++){
				if(primality(currValue++)){
					drawPixel(x, y);
				}
				x += dx;
				y += dy;
			}
		}

		drawLine(0, -1, len - 1);
		drawLine(-1, 0, len);
		drawLine(0, 1, len);
		drawLine(1, 0, len + 1);
	}
}

ulamSpiral(300);
