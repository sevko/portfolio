/**
 * Functions for rendering the Ulam and Sacks spirals.
 */

/**
 * Render an Ulam spiral on the page's canvas.
 *
 * @param {number} numLayers The number of concentric rings, or layers, in the
 *      spiral.
 */
function ulamSpiral(numLayers){
	"use strict";

	var drawPixel = setupCanvas(numLayers);

	var currValue = 1;
	var x = 0;
	var y = 0;

	/**
	 * Draw a line with a specified length and delta vector.
	 *
	 * @param dx The change in x between points.
	 * @param dy The change in y between points.
	 * @param len The number of points on the line.
	 */
	function drawLine(dx, dy, len){
		for(var pixel = 0; pixel < len; pixel++){
			if(primality(currValue++)){
				drawPixel(x, y);
			}
			x += dx;
			y += dy;
		}
	}

	for(var layer = 0, len = 0; layer <= numLayers; layer++, len += 2){
		drawLine(0, -1, len - 1);
		drawLine(-1, 0, len);
		drawLine(0, 1, len);
		drawLine(1, 0, len + 1);
	}
}

/**
 * Render a Sacks spiral on the page's canvas.
 *
 * @param {number} numLayers The number of squared values along the Sacks
 *      spiral's "axis."
 */
function sacksSpiral(numLayers){
	"use strict";

	var drawPixel = setupCanvas(numLayers);

	var currValue = 1;
	for(var layer = 1; layer <= numLayers; layer++){
		var numPoints = 2 * layer + 1;
		var angle = 2 * Math.PI / numPoints;
		for(var point = 1; point <= numPoints; point++){
			if(primality(currValue++)){
				var theta = point * angle;
				var radius = layer + point / numPoints;
				var x = Math.cos(theta) * radius;
				var y = Math.sin(theta) * radius;
				drawPixel(Math.floor(x), Math.floor(y));
			}
		}
	}
}

ulamSpiral(200);
// sacksSpiral(200);
