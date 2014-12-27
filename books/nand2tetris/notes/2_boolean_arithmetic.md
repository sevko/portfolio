# chapter 2: boolean arithmetic
The value of the string of digits $x_n x_{n-1} ... x_0$ in base $b$ is denoted $(x)_b$.

$$
(x)_b = \sum\limits_{i=0}^n x_i b^i
$$

## binary addition
Adding two binary numbers (numbers in base 2) is identical to adding numbers in base 10; each bit is a digit. Start
with the **LSB**, or *least significant bit*, and proceed to the **MSB**, or *most significant bit*.

```c
0 0 0 1
  1 0 0 1
+ 0 1 0 1
---------
  1 1 1 0

1 1 1 1
  1 0 1 1
+ 0 1 1 1
---------
1 0 0 1 0
```

## signed binary numbers
An ideal implementation of *signed binary numbers*, or binary numbers that allow the representation of both positive
and negative values, maps cleanly onto existing hardware solutions for adding only positive numbers: this allows us to
support one, generalized chip per each of addition, subtraction, etc. The most common system of representing signed
numbers is *2's complement*, or *radix complement*. The *2's complement* $\overline{x}$ of a number $x$ is:

$$
\overline{x} = \left\{
	\begin{array}{lr}
		2^n - x & \text{if } x \neq 0\\
		0 & \text{otherwise}
	\end{array}
\right.
$$

In a 5-bit system, $-2 = 2^5 - 2 = 32 - 2 = 30 = 11110$.

In short, to attain the *2's complement* $\overline{x}$ of a number $x$, negative all its bits (setting `0` to `1` and
`1` to `0` -- most programming languages represent this bitwise operation with the `~` operator), and add 1:

$$
\overline{x} = ~x + 1
$$

Here's an example of a 4-bit system:

\+ num | + bin | - num | - bin
--- | --- | --- | ---
0 | `0000` | - | -
1 | `0001` | `1111` | -1
2 | `0010` | `1110` | -2
3 | `0011` | `1101` | -3
4 | `0100` | `1100` | -4
5 | `0101` | `1011` | -5
6 | `0110` | `1010` | -6
7 | `0111` | `1001` | -7
- | - | `1000` | -8

In an $n$-bit system:

  1. there are $2^n$ values, in the inclusive range $[-2^{n - 1}, 2^{n - 1} - 1]$
  2. all positive numbers have a most significant bit of `0`
  3. all negative numbers have a most significant bit of `1`

As mentioned previously, an ideal system of signed numbers is usable with existing hardware, methods, etc.; *2's
complement* works out-of-the box with basic math/logic operations. For instance, adding negative numbers:

$$
-2 + -3 = 1110 + 1101 = 1011 = -5
$$

```c
1 1 0 0
  1 1 1 0
+ 1 1 0 1
---------
1 1 0 1 1
```

We'll implement the following adders:

  1. **Half adder**: add 2 bits.
  2. **Full adder**: add 3 bits.
  3. **Adder**/**Multi-bit adder**: add two $n$-bit numbers.
