# power set 2
A Node module containing a second implementation of an algorithm that finds the
[power set](http://en.wikipedia.org/wiki/Power_set) of a set (check out the [first](../power_set)).

```javascript
> var generateSubsets = require("power_set_2");
> console.log(generateSubsets([1, 2, 3]));
[ [  ], [ 1 ], [ 2 ], [ 1, 2 ], [ 3 ], [ 1, 3 ], [ 2, 3 ], [ 1, 2, 3 ] ]
```

The module contains Mocha unit-tests. To execute them:

```bash
$ mocha
```
