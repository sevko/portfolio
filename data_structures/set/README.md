# set
An implementation of the [set](http://en.wikipedia.org/wiki/Set_(abstract_data_type)) data-structure. `index.js`
contains the `Set` definition, which supports common set operations like `.insert()`, `.remove()`, `.union()`,
`.intersect()`, and `difference()`. Here's an example of `Set` usage:

```javascript
var Set = require("set"); // Note: this overrides the Javascript built-in Set().
var a = new Set(1, 2, 3);
var b = new Set(3, 4, 5);
var c = a.union(b);
console.log(c.isEqual(new Set(1, 2, 3, 4, 5))); // true
```

To run the module's [Mocha](http://visionmedia.github.io/mocha/) unit-test suite, simply execute:

```bash
$ mocha
```
