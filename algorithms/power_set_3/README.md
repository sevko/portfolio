# power set 3
A Node module containing a third implementation of an algorithm that finds the
[power set](http://en.wikipedia.org/wiki/Power_set) of a set (check out the [first](../power_set)
[two](../power_set_2)).

```javascript
> var powerSet = require("power_set");
> console.log(powerSet([1, 2, 3]));
[ [  ], [ 1 ], [ 2 ], [ 1, 2 ], [ 3 ], [ 1, 3 ], [ 2, 3 ], [ 1, 2, 3 ] ]
```

The module contains Mocha unit-tests. To execute them:

```bash
$ mocha
```
