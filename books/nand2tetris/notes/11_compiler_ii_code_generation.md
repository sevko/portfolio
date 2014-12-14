# chapter 11: compiler II: code generation
Programs care about:

  * variable type: `int`, `char`, `bool`, etc.
  * scope/lifespan: `global`, `static`, etc.

## symbol tables
Compilers keep track of identifier data using a **symbol table**, which might look like:

   name    |  type  |  kind  |  #
 --------- | ------ | ------ | ---
    id     |  int   | field  |  0
   name    | string | field  |  1

Symbol tables have to record identifiers' scopes. A list of *hash tables* might be used to model successive scopes.

## variables in memory
An *array* is usually stored as a sequence of memory locations with a pointer to the base address. The **Operating
System** might expose an `alloc(size)` function which appropriates `size` bytes of memory and returns the address of
the first byte. *Objects* are fundamentally similar to arrays, containing instance variables which can be accessed by
index from the object's base memory address.

*Class methods* are a different beast, since only one copy is required per class (regardless of the number of instances
) but must operate on specific instances. A pointer to that instance should be passed as an implicit argument:

```
a.b() -> b(a)
a.b(1, 2, 3) -> b(a, 1, 2, 3)
```

## expression evaluation, flow control
To evaluate an expression like `x + g(2, y, -2) * 5`, first convert it to *postfix*, or *Reverse Polish Notation*
(**RPN**). Examples: `f(x, y)` becomes `x, y, f`, `x + y` becomes `x, y, +`.

```
  +
 / \
x   *
   / \
  g   5
 /|\
2 y -
    |
    5
```

Then, compile to *Virtual Machine* code:

```
push x
push 2
push y
push 5
neg
call g
push 5
call multiply
add
```
