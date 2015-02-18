# chapter 9: evolution of code
Our contemporary understanding of genetic algorithms is extensively due to John Holland.

**Infinite monkey theorem**: a monkey randomly mashing a keyboard will eventually type out the complete works of
Shakespeare.

There are three kinds of genetic algorithms: *traditional* genetic algorithms, *interactive selection*, and *ecosystem
simulaton*.

An evolutionary system must support:

  1. **heredity**: passing of traits from parent to child
  2. **variation**: spontaneous variation of traits
  3. **selection**: a means for fit actors to reproduce more than unfit counterparts

The *genotype* is the genetic data itself, while the *phenotype* is the representation of the genotype.

Selection:

  1. evaluate fitness with a *fitness function*
  2. create a *mating pool* of eligible parents based on fitness

Reproduction:

  1. *crossover*: mix the parents' genotypes
  2. *mutation*; defined by the *mutation rate*, which indicates how frequently it occurs. Promotes variety in the
    genotype.
