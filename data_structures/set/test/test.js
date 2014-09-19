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
	});
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

describe("union()", function(){
	it("Should create a union of two sets.", function(){
		var set1 = new Set(1, 2, 3, 4);
		var set2 = new Set(4, 5, 6, 7);
		var union = set1.union(set2);
		assert(union.isEqual(new Set(1, 2, 3, 4, 5, 6, 7)));
	});
});

describe("intersection()", function (){
	it("Should create an intersection.", function (){
		var set1 = new Set(3, 4, 5, 6);
		var set2 = new Set(1, 2, 3, 4);
		assert(set1.intersection(set2).isEqual(new Set(3, 4)));
	});
});

describe("difference()", function (){
	it("Should create a difference.", function (){
		var set1 = new Set(3, 4, 5, 6);
		var set2 = new Set(1, 2, 3, 4, 7);
		assert(set1.difference(set2).isEqual(new Set(5, 6)));
		assert(set2.difference(set1).isEqual(new Set(1, 2, 7)));
	});
});

describe("isSubset()", function (){
	it("Should detect subsets.", function (){
		var set1 = new Set(3, 4, 5, 6);
		var set2 = new Set(4, 5);
		assert(set1.isSubset(set2));
	});
	it("Should detect non-subsets.", function (){
		var set1 = new Set(3, 4, 5, 6);
		var set2 = new Set(1, 2, 3);
		assert(!set1.isSubset(set2));
	});
});

describe("isEqual()", function (){
	it("Should identify equal sets.", function (){
		var set1 = new Set(1, 3, 2);
		var set2 = new Set(2, 1, 3);
		assert(set1.isEqual(set2));
	});
	it("Should identify unequal sets.", function (){
		var set1 = new Set(1, 3, 2);
		var set2 = new Set(4, 1, 3);
		assert(!set1.isEqual(set2));
	});
});

describe("toString()", function (){
	it("Should return a correct string.", function (){
		var items = [1, 2, 3, 4, 5];
		var set = new Set();
		for(var ind = 0; ind < items.length; ind++)
			set.insert(items[ind]);
		assert(items.toString() === set.toString());
	});
});
