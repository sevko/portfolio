# chapters 10/11: jack compiler
A Python implementation of a compiler that translates the *nand2tetris* object-oriented **Jack** language to **Hack**
virtual machine instructions.

```
python -m jackcompiler PATH
```

Arguments:

  1. **PATH**: the path to either a single Jack file, or a directory all of whose files are assumed to be Jack files.
    Will be compiled into files with identical basenames in the current directory.
