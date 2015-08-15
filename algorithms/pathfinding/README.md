# pathfinding: A* and Dijkstra's
A Python implementation of both [Dijkstra's Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) and
[A*](https://en.wikipedia.org/wiki/A*_search_algorithm) for a rectangular grid (with both impassable tiles and tiles
with variable movement costs). Here's a sample route:

```
1 1 1 1 1 █ 1 1 1 █ 1 1
* * * * * 9 9 █ █ █ 1 1
* █ █ █ * █ 9 █ 1 1 1 1
* █ 1 1 * █ 9 * * * * 1
* 1 █ 1 A █ 1 * █ █ * 1
* █ █ █ 9 █ 1 * █ 1 * 1
* * * * 9 1 █ * █ 4 * 9
1 1 █ * * * * * █ 5 * 9
1 1 █ 1 █ █ █ █ █ 5 * *
1 1 █ 1 █ 1 █ 1 █ 5 7 *
1 1 █ 1 1 1 █ 1 █ 1 1 B
```

### running
Simply run `python pathfinding.py`, which will output a ton of diagnostic information about each of the two algorithms
(displaying routes on a sample map, comparing performance both in terms of time and number of visited nodes, etc).

### acknowledgement
Red Blob Games's [Introduction to A*](http://www.redblobgames.com/pathfinding/a-star/introduction.html) was a
*fantastic* introduction to pathfinding, and I borrowed bits of its implementations of either algorithm.
