# array-queue
A [queue](http://en.wikipedia.org/wiki/Queue_%28abstract_data_type%29) implemented using an array, rather than a
singly-linked list, to allow amortized `O(1)` `enqueue()`, `dequeue()`, and `peekIndex()` (which, in singly-linked list
versions, is usually an `O(n)` operation) complexities.

###
To run the unit-tests:

```
make run
```
