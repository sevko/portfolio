# JSON parser
A JSON parser implemented according to the [JSON spec](http://json.org/), in Haskell with the
[parsec](https://hackage.haskell.org/package/parsec) library. Special care was taken to provide user-friendly error
reporting.

  * install dependencies with `cabal install`
  * run with `cabal run`:
  ```
  $ cabal run
  Preprocessing executable 'json_parser' for json-parser-0.0.1...
  Running json_parser...
  Enter JSON below:
  {
    "a": [1e5, 1.0, [3]],
    "foo": "ba\tr",
    "e\n": {"e": null}
  }
  Successfully parsed: {
          "a": [
                  100000,
                  1.0,
                  [
                          3
                  ]
          ],
          "foo": "ba\tr",
          "e
          ": {
                  "e": null
          }
  }
  ```

  ```
  $ cabal run
  Preprocessing executable 'json_parser' for json-parser-0.0.1...
  Running json_parser...
  Enter JSON below:
  {
    "a": 1e
  Parser error on line 2, column 10:

    1 |{
    2 |  "a": 1e
                ^
  Error:
  unexpected "\n"
  expecting "-", "+" or digit
  ```
