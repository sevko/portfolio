# chapter 7: vmtranslator
The first iteration of a Virtual Machine translator for the *nand2tetris* **Hack** computer, which implements:

  * stack arithmetic
  * memory access
  * control flow
  * function calling

The following will compile either a single Virtual Machine source code file, or directory of such files, to a raw
assembly file.

```
python -m vmtranslator SOURCE.asm|SOURCE_DIR
```
