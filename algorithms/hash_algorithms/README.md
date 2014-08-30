# Hash algorithms
This directory contains an implementation of **SHA-1** and the **SHA-2** suite of hash algorithms, in files listed in
the following table:

Algorithm | File
--- | ---
SHA-1 | sha1.py
SHA-224 | sha224.py
SHA-256 | sha256.py
SHA-384 | sha384.py
SHA-512/224 | sha512-224.py
SHA-512/256 | sha512-256.py
SHA-512 | sha512.py

All implementations, with the exceptions of **SHA-512/224** and **SHA-512/256**, which have hard-coded expected output
values, are tested against the corresponding functions in Python's `hashlib`. As per the module-level docstrings in
each file, these algorithms were written along the lines of the pseudocode provided on the relevant Wikipedia articles:

  * [SHA-1](http://en.wikipedia.org/wiki/SHA-1)
  * [SHA-2](http://en.wikipedia.org/wiki/SHA-2)

I checked the articles themselves against some official sources, like the relevant RFCs, but decided to adhere to the
pseudocode as much as possible for high readability: thus, the source is sometimes not PEP8 compliant, and has some..
occasionally questionable control flow.

Running any of the files will execute its inbuilt unit-test:

```bash
$ python sha1.py
Hash of 'The quick brown fox jumps over the lazy dog': 273069992013452546326057769888623105462687230738
Matches expected: True.
```
