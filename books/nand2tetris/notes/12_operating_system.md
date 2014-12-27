# chapter 12: operating system
The **Hack** *operating system*:

  1. interfaces with the computer's hardware and provides a programmer-friendly device API
  2. extends high-level languages with functions and data types

## multiplication
A naive algorithm for the multiplication of two numbers `a` and `b` might find the sum of `b` occurrences of `a`, and
look something like this:

```
for step in range(b):
	product += a
```

That approach has a runtime proportional to the *value* of `b`, which is awful considering that a signed integer on a
64-bit machine machine has a maximum value of 9,223,372,036,854,775,808. We instead want proportionality to the
bit *length* of `b`. For each bit in `b`, shift `a` left by 1 and, if the bit is 1, add it to the sum total.

```
sum = 0
shifted_a = a
for bit in b:
	if bit == 1:
		sum += shifted_a
	shifted_a <<= 1
```

## division
To divide `a` by `b`, iteratively subtracting `a` from `b` is similarly slow. Instead, perform a binary approximation:

```
def divide(a, b):
	if b > a:
		return 0

	q = 2 * divide(a, 2 * b)
	if a - (q * b) < b:
		return q
	else:
		return q + 1
```

## square root
Efficiently find square roots by performing a binary search:

```
def sqrt(x):
	root = 0
	for bit in range(n / 2 - 1, 0, -1):
		sum = root + 2 ** bit
		if sum ** 2 <= x:
			root = sum
```

## memory allocation
Variable lifetime differs by scope: `local` variables live and die on the stack, `static` variables may be allocated at
compile-time, etc. Allow allocation of blocks on the heap by maintaining a linked-list of free segments.

```
def alloc(size):
	segment = find_segment(size)
	if segment.length == size:
		remove(segment)
		segment[-1] = size
		return segment
	else:
		segment.length -= (size + 1)
		block = segment + segment.length - size
		block[-1] = size
		return block

def dealloc(ptr):
	add_segment(ptr)
```

## I/O
*Device drivers* are APIs for interacting with the computer's physical devices.

## pixel drawing
Computers use *bitmap*, or *raster*, images. The primitive operation is plotting a pixel on the screen. Pixels'
`x` coordinates increase from left to right, and `y` coordinates increase from top to bottom. In the *Hack* computer,
pixel plotting is accomplished by writing appropriate values to bytes in the screen's memory map.


## character rendering
The screen has to be divided into discrete character containers. 256 rows x 512 columns with 11x8 pixel characters
yields 23 lines of 64 characters each.
