// "use strict";

!function setup(){
	var img = new Image();
	img.src = "image.png";
	img.onload = function(){
		var canvas = document.getElementsByTagName("canvas")[0];
		canvas.width = img.width;
		canvas.height = img.height;

		var context = canvas.getContext("2d");
		context.drawImage(img, 0, 0, img.width, img.height);

		setInterval(function(){
			var oldImg = context.getImageData(0, 0, canvas.width, canvas.height);
			var newImg = context.createImageData(oldImg.width - 1, oldImg.height);
			canvas.width--;
			var gradient = getGradient(oldImg);
			var seam = getSeam(oldImg.height, oldImg.width, gradient);
			for(var ind = 0; ind < seam.length; ind++){
				oldImg.data[seam[ind] * 4] = 0xFF;
				oldImg.data[seam[ind] * 4 + 1] = 0;
				oldImg.data[seam[ind] * 4 + 2] = 0;
				oldImg.data[seam[ind] * 4 + 3] = 0xFF;
			}
			context.putImageData(oldImg, 0, 0);

			function theFuck(){
				carveSeam(oldImg, newImg, seam);
				context.putImageData(newImg, 0, 0);
				console.log(abc);
			}
			setTimeout(theFuck, 700);
		}, 1000);

		// var pix = oldImg.data;
		// for(var ind = 0, offset = 0; ind < gradient.length; ind++, offset += 4){
			// pix[offset] = gradient[ind] / 20;
			// pix[offset + 1] = 0;
			// pix[offset + 2] = 0;
			// pix[offset + 3] = 0xFF;
		// }
		// context.putImageData(oldImg, 0, 0);
	};
}();

function carveSeam(oldImg, newImg, seam){
	var oldPix = oldImg.data;
	var newPix = newImg.data;
	var breakpoint = seam.pop();
	for(
		var ind = 0, newOffset = 0;
		ind < oldImg.width * oldImg.height;
		ind++, newOffset += 4){
		while(ind === breakpoint){
			ind++;
			breakpoint = seam.pop();
		}
		var oldOffset = ind * 4;
		newPix[newOffset] = oldPix[oldOffset];
		newPix[newOffset + 1] = oldPix[oldOffset + 1];
		newPix[newOffset + 2] = oldPix[oldOffset + 2];
		newPix[newOffset + 3] = oldPix[oldOffset + 3];
	}
}

/**
 * Identify the least valuable seam of pixels in an image gradient.
 *
 * @param {number} height The (int) height of the image whose gradient is
 *      contained in `gradient`.
 * @param {number} width The (int) width of the image whose gradient is
 *      contained in `gradient`.
 * @param {Array of number} gradient An image gradient, containing the
 *      intensity of any particular pixel in an image (see `getGradient()`).
 */
function getSeam(height, width, gradient){
	var sumGrad = gradient.slice();
	for(var y = 1; y < height; y++){
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

	var seam = new Array(height);
	var minParentInd = sumGrad.length - width;
	for(var x = minParentInd + 1; x < sumGrad.length; x++){
		if(sumGrad[x] < sumGrad[minParentInd]){
			minParentInd = x;
		}
	}
	seam[0] = minParentInd;
	for(var row = 1; row < height; row++){
		var currInd = seam[row - 1];
		var parentInd = currInd - width;

		var parents = {};
		parents[sumGrad[parentInd]] = parentInd;
		if(currInd % width > 0){
			parents[sumGrad[parentInd - 1]] = parentInd - 1;
		}

		if(currInd % width < width - 1){
			parents[sumGrad[parentInd + 1]] = parentInd + 1;
		}

		seam[row] = parents[Math.min.apply(null, Object.keys(parents))];
		if(minParentInd < 0){
			console.log('what the fuck?', currInd, parentInd, row);
		}
	}
	console.log('width', width, 'height', height);

	return seam;
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
