/**
 * @file Unit tests for the Set implementation in `index.js`.
 */

"use strict";

var assert = require("assert");
var Set = require("../index.js"); // jshint ignore:line

describe("Set()", function(){
	it("Should insert arguments.", function(){
		var set = new Set(1, 2, 3, 4);
		assert(set.length() == 4);
	});
});

describe("isMember()", function(){
	it("Should find existing items.", function(){
		var set = new Set();
		set.insert(1);
		assert(set.isMember(1));
	});
	it("Should not find inexistent items.", function(){
		var set = new Set();
		set.insert(2);
		assert(!set.isMember(1));
	});
});

describe("length()", function(){
	it("Should report number of items in the set.", function(){
		var set = new Set();
		assert(set.length() === 0);
		set.insert(1);
		assert(set.length() === 1);
	});
});

describe("insert()", function(){
	it("Should insert single items.", function(){
		var set = new Set();
		set.insert(1);
		assert(set.length() === 1);
	});
	it("Should insert multiple items.", function(){
		var set = new Set();
		set.insert(1, 2, 3, 4, 5);
		assert(set.length() === 5);
	});
	it("Should prevent duplicate insertions.", function(){
		var set = new Set();
		set.insert(1);
		assert(set.length() === 1);
		set.insert(2);
		assert(set.length() === 2);
		set.insert(1);
		assert(set.length() === 2);
	});
});

describe("remove()", function(){
	it("Should remove single items", function (){
		var set = new Set();
		set.insert(1);
		set.remove(2);
		assert(set.length() === 1);
		set.remove(1);
		assert(set.length() === 0);
	})
	it("Should remove multiple items.", function(){
		var set = new Set(1, 2, 3, 4);
		assert(set.length() == 4);
		set.remove(5);
		assert(set.length() == 4);
	});
	it("Should not remove inexistent items.", function(){
		var set = new Set();
		set.insert(1);
		set.remove(2);
		assert(set.length() === 1);
		set.remove(1);
		assert(set.length() === 0);
	});
});
