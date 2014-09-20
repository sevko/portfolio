/**
 * @file An implementation of a solution to the set-covering problem.
 */

"use strict";

var Set = require("../../data_structures/set/"); // jshint ignore:line

/**
 * Generate all subsets of a array.
 *
 * @param {array} array An array.
 * @return {array of arrays} The set of all subsets of `array`, ordered by
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

/**
 * Find the set cover for an array of sets.
 *
 * @param {array of sets} sets An array of sets.
 * @return {array of sets} The smallest subset of `sets`, whose union is equal
 *      to the union of all sets inside `sets`; in other words, the set cover
 *      for `sets`.
 */
function findSetCover(sets){
	var universe = new Set();
	for(var set = 0; set < sets.length; set++){
		universe = universe.union(sets[set]);
	}

	var subsets = generateSubsets(sets);
	for(var subset = 0; subset < subsets.length; subset++){
		var union = new Set();
		for(set = 0; set < subsets[subset].length; set++){
			union = union.union(subsets[subset][set]);
		}
		if(union.isEqual(universe)){
			return subsets[subset];
		}
	}
}
