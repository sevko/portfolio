# chapter 4: machine language
*Machine language*, which interfaces between hardware and software, is a formalism used to manipulate a *memory* using
a *processor* and a set of *registers*.

  * **Memory**: array of cells (*words*, or, alternatively, *locations*) with unique addresses.
  * **Processor**: the CPU. Performs basic arithmetic, logic, memory access, etc.
  * **Registers**: high access-speed data caches separate from Memory.

A machine language program is a series of bit-packed instructions, like  `1010001100011001`, which are interpreted
according to the computer's chip set. For instance, machine instructions for a particular CPU may take the form of four
4-bit fields:

 field  |  bits from ex.  |   designation
 ------ | --------------- | --------------
   1    |     `1010`      |  CPU operation
   2    |     `0011`      |   destination
   3    |     `0001`      |    operand 1
   4    |     `1001`      |    operand 2

Assembly languages are essentially human-readable representations of machine code that use mnemonics to represent
machine instructions, and are then converted to binary by a preprocessor called an **assembler**.  To extend our
example, here's a potential assembly representation of `1010001100011001`:

 binary  |  assembly
 ------- | ---------
 `1010`  |   `set`
 `0011`  |    `R3`
 `0001`  |    `R1`
 `1001`  |    `R2`

Every computer must support:

## arithmetic/boolean operations

eg

```asm
ADD R3, R1, R2
ADD R2, R1, foo
```

## memory access
`load` and `store` commands that move data between *registers* and *memory*, in the form of any of the following three
forms:

  * **direct addressing**: refer to a specific address in memory.

   ```
   LOAD R1, 67 // R1 <- Mem[67]
   LOAD R1, bar // R1 <- Mem[bar]
   ```

  * **immediate addressing**: load constant values as they appear in the instruction.

  ```
  LOAD R1, 67 // R1 <- 67
  ```

  * **indirect addressing**: load values at the location pointed to by the address pointed to by the argument; models
   pointers. For instance, accessing `foo[j]`:

   ```
   x = foo[j];
   x = *(foo + j)
   ```

   or

   ```
   ADD R1, foo, j
   LOAD* R2, R1
   STR R2, x
   ```

## control flow
Must support repetition (loops), conditional execution (branching), subroutines (functions).
