/**
 * @file Unit tests for the the `set_cover` module.
 */

"use strict";

var assert = require("assert");
var Set = require("../../../data_structures/set/"); // jshint ignore:line
var findSetCover = require("../index.js");

describe("findSetCover()", function(){
	it("should find the set cover for a given set", function(){
		var sets = [
			new Set(1, 2),
			new Set(3),
			new Set(1, 2, 3),
			new Set(4, 5, 6),
			new Set(6)
		];
		var setCover = findSetCover(sets);
		var expected = [
			new Set(4, 5, 6),
			new Set(1, 2, 3)
		];

		assert(setCover.length == expected.length);
		for(var set = 0; set < setCover.length; set++){
			assert(setCover[set].isEqual(expected[set]));
		}
	});
});
