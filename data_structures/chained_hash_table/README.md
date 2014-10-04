# chained hash table

A Python implementation of a [chained hash table](http://en.wikipedia.org/wiki/Hash_table#Separate_chaining_with_linked_lists).
Sample usage:

```python
import chained_hash_table
num_buckets = 5
def hash_func(x):
	return x % 5
table = chained_hash_table.ChainedHashTable(num_buckets, hash_func)
table.insert(2)
```

The module has `unittest` unit-tests; to execute them, `python test.py`.
