# chapter 7: virtual machine I: stack arithmetic
**virtual machine**: an abstract computer that can be implemented on top of a hardware platform, and thus allows for
portable high-level code. Typically has its own language, in which you can write *VM programs*. Supports:

  * arithmetic
  * memory access
  * program flow
  * subroutines

Virtual machines provide a separation of concerns between high-level language interpretation and low-level, hardware
specific compilation.

  * **Pascal** compilers in the 70s generated *p-code*
  * **Java** compilers generate *bytecode*
  * **.NET** generates an *intermediate language* (*IR*) that runs on the *Common Language Runtime*.

Key question: where do we put the operands and results of VM operations? One solution, the *stack machine module*, uses
a stack (*LIFO*, or Last In First Out, data structure), <b>pop</b>ping operands and <b>push</b>ing results.

Virtual machine paradigm:

  * high-level language
  * *compiler*
  * VM language
  * *VM translator*
  * assembly
  * *assembler*
  * machine language
  * hardware
