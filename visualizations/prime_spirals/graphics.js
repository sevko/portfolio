/**
 * Configure the HTML canvas for a certain number of layers.
 *
 * @param {number} numLayers The number of layers (concentric rings) in the
 *      desired spiral. Will serve as half the height and width of the
 *      canvas
 * @return  {function(x, y)} A function that intakes x- and y-coordinates and
 *      renders them at an offset equal to `numLayers` (half the height/width).
 */
function setupCanvas(numLayers){
	var sideLen = numLayers * 2 + 1;
	var canvas = document.getElementsByTagName("canvas")[0];
	canvas.setAttribute("width", sideLen);
	canvas.setAttribute("height", sideLen);

	var context = canvas.getContext("2d");
	var halfSideLen = (sideLen - 1) / 2;
	return function drawPixel(x, y){
		context.fillRect(x + halfSideLen, y + halfSideLen, 1, 1);
	};
};
