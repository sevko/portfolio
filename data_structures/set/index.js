/**
 * @file An implementation of a Set data-structure.
 */

function test(){
	console.log("Testing.");
}

/**
 * Create a Set data-structure, which allows all mathematical set operations
 * and prevents data duplication.
 *
 * @constructor
 * @return {Set} An empty set.
 */
function Set(){
	this.items = [];

	/**
	 * Insert an item into this Set, if it's not already present.
	 *
	 * @param {*} item The item to attempt inserting.
	 */
	this.insert = function insert(item){
		if(!this.isMember(item))
			this.items.append(item);
	};

	/**
	 * Remove an item from this Set, if it's present.
	 *
	 * @param {*} item The item to attempt removing.
	 */
	this.remove = function remove(item){
		var ind = this.items.indOf(item);
		if(ind != -1)
			this.items.splice(ind, 1);
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
}

module.exports = Set;
test();
