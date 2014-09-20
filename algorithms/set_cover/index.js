"use strict";

function subsets(array){
	function fixedLengthSubsets(subsetLen, itemNum, startInd){
		if(itemNum === subsetLen){
			return [[]];
		}
		else {
			var subsets = [];
			var endInd = array.length - (subsetLen - itemNum - 1);
			for(var ind = startInd; ind < endInd; ind++){
				var fixedSubsets = fixedLengthSubsets(
					subsetLen, itemNum + 1, ind + 1
				);
				for(var subset = 0; subset < fixedSubsets.length; subset++){
					fixedSubsets[subset].push(array[ind]);
					subsets.push(fixedSubsets[subset]);
				}
			}
			return subsets;
		}
	}

	var subsets = [];
	for(var subsetLen = 1; subsetLen <= array.length; subsetLen++){
		var fixedLenSubsets = fixedLengthSubsets(subsetLen, 0, 0);
		for(var subset = 0; subset < fixedLenSubsets.length; subset++){
			subsets.push(fixedLenSubsets[subset]);
		}
	}
	return subsets;
}

console.log(subsets([1, 2, 3, 4]));
