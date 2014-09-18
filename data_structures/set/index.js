/**
 * @file An implementation of a Set data-structure.
 */

"use strict";

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

	this.union = function union(set){
	};

	this.intersection = function intersection(set){
	};

	this.difference = function difference(set){
	};

	/**
	 * @param {*} item The item to search for.
	 * @return {boolean} Whether or not `item`is contained in this Set.
	 */
	this.isMember = function isMember(item){
		return this.items.indexOf(item) != -1;
	};

	this.isSubset = function isSubset(set){
	};

	this.isEqual = function isEqual(set){
	};

	/**
	 * @return {int} The number of items contained inside this Set.
	 */
	this.length = function length(){
		return this.items.length;
	};

	this.insert.apply(this, arguments)
}

module.exports = Set;
