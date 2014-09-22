/**
 * @file An implementation of an algorithm that finds the power set of a set.
 */

"use strict";

/**
 * Generate all subsets of an array.
 *
 * Uses the binary form of an integer to indicate which values are present in
 * any subset of the given set.
 *
 * @param {array} array An array.
 * @return {array} The set of all subsets of `array`.
 */
function powerSet(array){
	var subsets = [];
	for(var subset = 0; subset < Math.pow(2, array.length); subset++){
		var newSubset = [];
		for(var bit = 0; bit < array.length; bit++){
			if((subset >> bit) & 1){
				newSubset.push(array[bit]);
			}
		}
		subsets.push(newSubset);
	}

	return subsets;
}

module.exports = powerSet;
