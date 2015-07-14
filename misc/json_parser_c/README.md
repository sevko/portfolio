# JSON parser
A recursive descent JSON parser written according to the [JSON spec](http://json.org/), in C. The files of interest are
[`json_parser.c`](src/json_parser.c) (and [`json_parser.h`](src/json_parser.h)), which contain the parser
implementation and are extensively documented.

## compile and run tests

  * `make all`: compile the parser
  * `make run`: run unit-tests

## limitations:
The parser has several limitations:

  * doesn't handle big numbers (all numeric values are either `int`s or `float`s), so overflow might occur
  * only supports UTF8
