# subset generator
A Node module containing an implementation of a simple recursive algorithm that finds all the
[subsets](http://en.wikipedia.org/wiki/Subset) of a set. Sample usage:

```javascript
> var generateSubsets = require("generate_subsets");
> console.log(generateSubsets([1, 2, 3]));
[ [ 1 ], [ 2 ], [ 3 ], [ 2, 1 ], [ 3, 1 ], [ 3, 2 ], [ 3, 2, 1 ] ]
```

The module contains Mocha unit-tests. To execute them:

```bash
$ mocha
```
