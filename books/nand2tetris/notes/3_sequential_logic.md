# chapter 3: sequential logic
The chips implemented in chapters **1** and **2** are *combinational*, implementing functions that act on two arguments
irrespective of *time*; they're stateless. Memory elements, which *can* maintain state, are built from *sequential
chips* and are **time-dependent**.

Low-level sequential gates are called *flip-flops*, and are the primitive building blocks of:

  * binary cells
  * registers
  * memory banks
  * counters
  * etc.

**Computer clocks** are modeled by oscillations; the time between each oscillation, or the counter's period, is called
the *clock cycle*.

Two important gates:

  * **DFF**, or *<b>d</b>ata <b>f</b>lip-<b>f</b>lop*: $out(t) = in(t - 1)$
  * **Register**: $out(t) = out(t - 1)$.

A multi-bit register has a *width*, indicating the number of bits that it stores. The content of a register is called
a *word*.

A **RAM**, or *<b>r</b>andom <b>a</b>ccess <b>m</b>emory*, device must allow random addresses to be accessed in the
same amount of time, and accepts the following inputs:

  * `load`: a bit that indicates whether to *read* or *write* data
  * `address`: the address to access
  * `data`: the data to write to `address`, if `load` is set.

Its design parameters include:

  * `width`: the number of bits per register
  * `size`: the number of registers

Sequential chips, roughly, are chips that contains **DFF**s internally. They rely on feedback loops, which are
recursively problematic in combinational chips as the output relies on itself, but can be successfully implemented in
sequential circuits because output depends on *previous* output. They're unstable during clock cycles.
