# power set
A Node module containing an implementation of a simple recursive algorithm that finds the
[power set](http://en.wikipedia.org/wiki/Power_set) of a set. Sample usage:

```javascript
> var powerSet = require("power_set");
> console.log(powerSet([1, 2, 3]));
[ [ 1 ], [ 2 ], [ 3 ], [ 2, 1 ], [ 3, 1 ], [ 3, 2 ], [ 3, 2, 1 ] ]
```

The module contains Mocha unit-tests. To execute them:

```bash
$ mocha
```
