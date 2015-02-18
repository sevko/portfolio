# chapter 8: fractals
Benoit Mandelbrot coined "fractal" in 1975. A *fractal* is a rough or fragmented geometric shape that can be split into
parts, each of which is a reduced-size copy of the whole.

Any branch of a fractal tree resembles the three when viewed from its parent (it exhibits self-similarity). A
stochastic fractal is built out of probabilities and randomness. Self-similarity is statistical. Fractals are
recursive, and have finely-detailed structures at any level.

1883: George Cantor devised simple rules for generating an infinite set, called the *Cantor set*:

  1. start with a line
  2. remove the middle third
  3. repeat for the two children

1904: Helge von Koch patented the *Koch curve*:

  1. start with a line
  2. divide the line into three parts
  3. draw an equilateral triangle using the middle segment as its base
  4. erase the base of the equilateral triangle
  5. repeat for all lines

The Koch curve and other similar fractals are called *mathematical monsters*; their lengths tend towards infinity in a
finite area.

Lindenmayer founded *L-systems*, which consist of:

  * an **alphabet**: the valid characters, or members, of the system.
  * **rules**: statements indicating what members any given member(s) will evolve into. Specify how the system evolves.
  * an **axiom**: the starting state of the system (ie, the first generation of members).
