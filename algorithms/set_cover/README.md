# set cover problem
An implementation of a simple [set covering](http://en.wikipedia.org/wiki/Set_cover_problem) algorithm, which generates
all the subsets of the given set and then checks the union of the sets contained within each against the "universe", or
union of the given set. Relies on the `Set` data-structure, implemented in the `set` module in this repository
(`/data_structures/set/'`). Sample usage:

```javascript
var findSetCover = require("set_cover");
var Set = require("set");
var sets = [
	new Set(1, 2),
	new Set(2, 3),
	new Set(1, 2, 3)
];
findSetCover(sets); // [1, 2, 3]
```

The module contains Mocha unit-tests. To execute them:

```bash
$ mocha
```
