/**
 * @file An implementation of an algorithm that finds a set's subsets.
 */

"use strict";

/**
 * Generate all subsets of an array.
 *
 * @param {array} array An array.
 * @return {array} The set of all subsets of `array`.
 */
function generateSubsets(array){
	var subsets = [[]];
	for(var item = 0; item < array.length; item++){
		var numExistingSubsets = subsets.length;
		for(var subset = 0; subset < numExistingSubsets; subset++){
			var newSubset = subsets[subset].slice(0);
			newSubset.push(array[item]);
			subsets.push(newSubset);
		}
	}
	return subsets;
}

module.exports = generateSubsets;
