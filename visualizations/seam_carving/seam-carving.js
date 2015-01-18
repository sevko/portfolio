"use strict";

!function setup(){
	var img = new Image();
	img.src = "image.png";
	img.onload = function(){
		var canvas = document.getElementsByTagName("canvas")[0];
		canvas.width = img.width;
		canvas.height = img.height;

		var context = canvas.getContext("2d");
		context.drawImage(img, 0, 0, img.width, img.height);

		var oldImg = context.getImageData(0, 0, canvas.width, canvas.height);
		var newImg = context.createImageData(img.width - 1, img.height);

		var gradient = getGradient(oldImg);
		gradient = carveSeam(oldImg, newImg, gradient);
		var pix = oldImg.data;
		for(var ind = 0, offset = 0; ind < gradient.length; ind++, offset += 4){
			pix[offset] = gradient[ind] / 20;
			pix[offset + 1] = 0;
			pix[offset + 2] = 0;
			pix[offset + 3] = 0xFF;
		}
		context.putImageData(oldImg, 0, 0);
	};
}();

// return RGBA array
function carveSeam(oldImg, newImg, gradient){
	var sumGrad = gradient.slice();
	var width = oldImg.width;
	for(var y = 1; y < oldImg.height; y++){
		for(var x = 0; x < width; x++){
			var ind = y * width + x;
			var parentInd = ind - width;
			var parents = [sumGrad[parentInd]];
			if(x > 0){
				parents.push(sumGrad[parentInd - 1]);
			}
			if(x < width - 1){
				parents.push(sumGrad[parentInd + 1]);
			}
			sumGrad[ind] += Math.min.apply(null, parents);
		}
	}

	return sumGrad;
}

/**
 * Compute the image gradient of a canvas.
 *
 * @param {ImageData} img The `ImageData` of the canvas to operate on.
 * @return {Array of number} The energy, intensity, or gradient of each pixel
 *      inside `img`, computed with `getColor()` on adjacent pixels in both the
 *      `x` and `y` directions. When no valid neighbor is present in any one
 *      direction (ie, when the pixel is on the edge of the canvas), the pixel
 *      itself is used as a substitute.
 */
function getGradient(img){
	var gradient = new Array(img.height * img.width);
	var pix = img.data;
	var width = img.width;
	var height = img.height;
	var rowOffset = img.width * 4;

	for(var y = 0; y < img.height; y++){
		for(var x = 0; x < img.width; x++){
			var ind = y * width + x;
			var offset = ind * 4;

			var xGradient = colorGradient(
				getColor(pix, (x === 0) ? offset : offset - 4),
				getColor(pix, (x === width - 1) ? offset : offset + 4)
			);
			var yGradient = colorGradient(
				getColor(pix, (y === 0) ? offset : offset - rowOffset),
				getColor(pix, (y === height - 1) ? offset : offset + rowOffset)
			);
			gradient[ind] = xGradient + yGradient;
		}
	}

	return gradient;
}

function getColor(pix, ind){
	return {
		red: pix[ind],
		green: pix[ind + 1],
		blue: pix[ind + 2],
	};
}

/**
 * @return {number} The magnitude of the gradient between `color1` and
 *      `color2`.
 */
function colorGradient(color1, color2){
	var diffR = color1.red - color2.red;
	var diffG = color1.green - color2.green;
	var diffB = color1.blue - color2.blue;
	return Math.sqrt(diffR * diffR + diffG * diffG + diffB * diffB) | 0;
}
