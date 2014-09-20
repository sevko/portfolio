/**
 * @file An implementation of an algorithm that finds a set's subsets.
 */

"use strict";

/**
 * Generate all subsets of an array.
 *
 * @param {array} array An array.
 * @return {array} The set of all subsets of `array`, ordered by
 *      length.
 */
function generateSubsets(array){
	/**
	 * Recursively generate all subsets of a certain length.
	 *
	 * @param {int} subsetLen The length of each of the generated subsets.
	 * @param {int} itemNum The number of the item located at `startInd` of the
	 *      superset array, relative to the numbers already added to the subset
	 *      by preceding calls to `fixedLengthSubsets()`. Used to moderate
	 *      recursion, which terminates when `itemNum` equals `subsetLen`.
	 * @param {int} startInd The index to start generating recursive subsets
	 *      from.
	 */
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

module.exports = generateSubsets;
