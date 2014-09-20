/**
 * @file An implementation of a Set data-structure.
 */

"use strict";

var util = require("util");

/**
 * Create a Set data-structure, which allows all mathematical set operations
 * and prevents data duplication.
 *
 * @constructor
 * @param {*} * Any arguments will be inserted into the newly created Set.
 * @return {Set} A set.
 */
function Set(){
	this.items = [];

	/**
	 * Insert items into this Set, if they're not already present.
	 *
	 * @param {*} item At least one item to attempt inserting. Any number of
	 *      additional arguments is accepted.
	 */
	this.insert = function insert(item){
		for(var arg = 0; arg < arguments.length; arg++)
			if(!this.isMember(arguments[arg]))
				this.items.push(arguments[arg]);
	};

	/**
	 * Remove items from this Set, if they're present.
	 *
	 * @param {*} items At least one time to attempt removing. Any number of
	 *      additional arguments is accepted.
	 */
	this.remove = function remove(item){
		for(var arg = 0; arg < arguments.length; arg++){
			var ind = this.items.indexOf(arguments[arg]);
			if(ind != -1)
				this.items.splice(ind, 1);
		}
	};

	/**
	 * Return the union of this Set and another.
	 *
	 * @param {Set} set Another Set instance to combine with this Set.
	 * @return {Set} A new Set, containing all of the elements inside this Set
	 *      and `set`, without duplicates.
	 */
	this.union = function union(set){
		var unionSet = new Set();
		this.insert.apply(unionSet, this.items);
		this.insert.apply(unionSet, set.items);
		return unionSet;
	};

	/**
	 * Return the intersection of this Set and another.
	 *
	 * @param {Set} set Another Set instance to intersect with this Set.
	 * @return {Set} A new Set, containing all the elements shared by this Set
	 *      and `set`.
	 */
	this.intersection = function intersection(set){
		var intersectionSet = new Set();
		for(var ind = 0; ind < this.items.length; ind++)
			if(set.isMember(this.items[ind]))
				intersectionSet.insert(this.items[ind]);
		return intersectionSet;
	};

	/**
	 * Return the difference of this Set and another.
	 *
	 * @param {Set} set Another Set instance to subtract from this Set.
	 * @return {Set} A new Set, containing all the elements inside this Set
	 *      that aren't inside `set`.
	 */
	this.difference = function difference(set){
		var differenceSet = new Set();
		for(var ind = 0; ind < this.items.length; ind++)
			if(set.items.indexOf(this.items[ind]) === -1)
				differenceSet.insert(this.items[ind]);
		return differenceSet;
	};

	/**
	 * @param {*} item The item to search for.
	 * @return {boolean} Whether or not `item`is contained in this Set.
	 */
	this.isMember = function isMember(item){
		return this.items.indexOf(item) != -1;
	};

	/**
	 * @param {Set} set A set.
	 * @return {boolean} Whether or not all of the items contained inside `set`
	 *      are contained inside this Set.
	 */
	this.isSubset = function isSubset(set){
		if(this.length() < set.length())
			return false;

		for(var ind = 0; ind < set.items.length; ind++)
			if(!this.isMember(set.items[ind]))
				return false;

		return true;
	};

	/**
	 * @param {Set} set The set to compare this Set against.
	 * @return {boolean} Whether or not the items in this Set are all contained
	 *      in `set`, and vice versa.
	 */
	this.isEqual = function isEqual(set){
		if(this.length() !== set.length())
			return false;

		for(var ind = 0; ind < this.items.length; ind++)
			if(set.items.indexOf(this.items[ind]) === -1)
				return false;

		return true;
	};

	/**
	 * @return {int} The number of items contained inside this Set.
	 */
	this.length = function length(){
		return this.items.length;
	};

	/**
	 * @return {string} The string representation of `this.items`.
	 */
	this.toString = function toString(){
		return util.format("[%s]", this.items.join(", "));
	};

	this.insert.apply(this, arguments);
}

module.exports = Set;
