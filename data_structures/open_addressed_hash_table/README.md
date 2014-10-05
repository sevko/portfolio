# open-addressed hash table
An implementation of an simple, dynamically-resizing
[open-addressed hash table](http://en.wikipedia.org/wiki/Hash_table#Open_addressing). Sample usage:

```python
import open_addressed_hash_table

def hash_func(key, num_probes):
	return key

table = open_addressed_hash_table.OpenAddressedHashTable(10, hash_func)
table.insert(2)
table.remove(2)
print table
```

This module contains `unittest` unit tests; to execute them, `python test.py`.
