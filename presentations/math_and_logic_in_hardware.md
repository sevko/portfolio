<link rel="stylesheet" href="http://files.sevko.io/presentations/style.css">

## math and logic at the hardware level

![](http://files.sevko.io/presentations/math_and_logic_in_hardware/logic_gate_diagram.png)

---

### but how does it know?!

```python
>>> True or False
True
>>> 1 == 1 and 10 < 30
True
>>> 30 + 2 - 5
27
```

---

### the software/hardware stack

  1. JavaScript, Python, Java
  2. C, C++, Rust
  3. ARM, x86, MIPS
  4. machine code
  5. CPU
  6. ALU
  7. logic gates

---

### boolean algebra

  * **binary** values: `true`|`false`, on/off, 0/+5 volts
  * *primitive* operations are `and`, `or`, and `not`.
  * *derived* operations:
    1. a `xor` b = (a `or` b) `and` `not`(a `and` b)
    2. a `eq` b = `not` (a `xor` b)
  * *truth tables* exhaustively list input/outputs:

a | b | and  | or
--- |  --- | --- | ---
0 | 0 | 0 | 0
1 | 0 | 0 | 1
0 | 1 | 0 | 1
1 | 1 | 1 | 1

---

### logic gates
  * *logic gates*: hardware implementations of boolean functions
  * *NAND*/*NOR* gates are universal

![](http://files.sevko.io/presentations/math_and_logic_in_hardware/logic_gate_symbols.png)

---

### logic gates (cont.)
  * made of transistors, [LEGOs](https://www.youtube.com/watch?v=5X_Ft4YR_wU),
    [redstone](https://www.youtube.com/watch?v=P7E4K5D834g), water, etc.)

![](http://files.sevko.io/presentations/math_and_logic_in_hardware/nand_gate_diagram.png)
![](http://files.sevko.io/presentations/math_and_logic_in_hardware/pascaline.png)
![](http://files.sevko.io/presentations/math_and_logic_in_hardware/stepped_reckoner.png)

---

### binary arithmetic

  * like regular addition: just carry the 1s

```python
      1
  1 0 0 1 = 9
+ 0 1 0 1 = 5
---------
  1 1 1 0 = 14

1 1 1 1
  1 0 1 1 = 11
+ 0 1 1 1 = 7
---------
1 0 0 1 0 = 18
```

---

### binary arithmetic

a | b | a + b | &#124; carry | sum | &#124; and | xor
--- | --- | --- | --- | --- | --- | ---
0 | 0 | 00 | &#124; 0 | 0 | &#124; 0 | 0
0 | 1 | 01 | &#124; 0 | 1 | &#124; 0 | 1
1 | 0 | 01 | &#124; 0 | 1 | &#124; 0 | 1
1 | 1 | 10 | &#124; 1 | 0 | &#124; 1 | 0

  * sum = a `xor` b, carry = a `and` b
  * **half-adder** (2 bits) → **full-adder** (3 bits) → **adder** (*n* bits)
