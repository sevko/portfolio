/**
 * @file An implementation of a solution to the set-covering problem.
 */

"use strict";

var Set = require("../../data_structures/set/"); // jshint ignore:line
var generateSubsets = require("../generate_subsets/");

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

/**
 * Run `findSetCover()` for a sample array of sets.
 */
(function testSetCover(){
	var sets = [
		new Set(1, 5),
		new Set(2),
		new Set(3),
		new Set(4)
	];

	console.log(findSetCover(sets).toString()); // 1 - 5
})();
