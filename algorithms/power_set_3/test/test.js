/**
 * @file Unit tests for the the `power_set_3` module.
 */

"use strict";

var assert = require("assert");
var powerSet = require("../index.js");

describe("powerSet()", function(){
	it("should generate all subsets of a set", function(){
		var subsets = powerSet([1, 2, 3]);
		var expected = [[], [1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]];

		// Relies on a precise order of the subsets in the returned set which,
		// given the definition of a set, doesn't make any sense, but this'll
		// do as a quick-and-dirty test.
		assert(subsets.length == expected.length);
		for(var subset = 0; subset < subsets.length; subset++){
			for(var ind = 0; ind < subsets[subset].length; ind++){
				assert(subsets[subset][ind] === expected[subset][ind]);
			}
		}
	});
});
