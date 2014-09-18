/**
 * @file Unit tests for the Set implementation in `index.js`.
 */

"use strict";

var assert = require("assert");
var Set = require("../index.js");

describe("insert()", function(){
	it("Should insert items.", function(){
		var set = new Set();
		set.insert(1);
		assert(set.length() == 1);
	});
	it("Should prevent duplicate insertions.", function(){
		var set = new Set();
		set.insert(1);
		assert(set.length() == 1);
		set.insert(2);
		assert(set.length() == 2);
		set.insert(1);
		assert(set.length() == 2);
	});
});

describe("remove()", function(){
	it("Should remove present items.", function(){
		var set = new Set();
		set.insert(1);
		set.remove(2);
		assert(set.length() == 1);
		set.remove(1);
		assert(set.length() == 0);
	});
});
