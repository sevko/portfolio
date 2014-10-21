# heap
An implementation of a [heap](http://en.wikipedia.org/wiki/Heap_(data_structure)). Sample usage:

```python
>>> import heap
>>> heap_ = heap.Heap()
>>> heap_.insert(*xrange(7))
>>> print heap_
6
3, 5
0, 2, 1, 4
>>> heap_.remove()
6
>>> print heap_
5
3, 4
0, 2, 1
```

The module has `unittest` unit-tests; to execute them, `python -m heap.test.test`.
