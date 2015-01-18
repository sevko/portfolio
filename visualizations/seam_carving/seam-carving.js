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
		var imgData = context.getImageData(0, 0, canvas.width, canvas.height);
		var newImg = context.createImageData(img.width - 1, img.height);
		carveSeam(imgData, newImg);
		context.putImageData(imgData, 0, 0);
	};
}();

// return RGBA array
function carveSeam(img, newImg){
	var gradient = getGradient(img);
	var pix = img.data;
	for(var ind = 0, offset = 0; ind < gradient.length; ind++, offset +=4){
		pix[offset] = gradient[ind];
		pix[offset + 1] = 0;
		pix[offset + 2] = 0;
		pix[offset + 3] = 0xFF;
	}
}

// return intensity array
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

// return gradient
function colorGradient(color1, color2){
	var diffR = color1.red - color2.red;
	var diffG = color1.green - color2.green;
	var diffB = color1.blue - color2.blue;
	return Math.sqrt(diffR * diffR + diffG * diffG + diffB * diffB) | 0;
}
