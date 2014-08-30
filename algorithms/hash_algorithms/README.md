# Hash algorithms
This directory contains an implementation of **SHA-1** and the **SHA-2** suite of hash algorithms, in files listed in
the following table:

Algorithm | File
--- | ---
SHA-1 | sha-1.py
SHA-224 | sha-224.py
SHA-256 | sha-256.py
SHA-384 | sha-384.py
SHA-512/224 | sha-512-224.py
SHA-512/256 | sha-512-256.py
SHA-512 | sha-512.py

All implementations, with the exceptions of **SHA-512/224** and **SHA-512/256**, which have hard-coded expected output
values, are tested against the corresponding functions in Python's `hashlib`. As per the module-level docstrings in
each file, these algorithms were written along the lines of the pseudocode provided on the relevant Wikipedia articles:

  * [SHA-1](http://en.wikipedia.org/wiki/SHA-1)
  * [SHA-2](http://en.wikipedia.org/wiki/SHA-2)

I checked the articles themselves against some official sources, like the relevant RFCs, but decided to adhere to the
pseudocode as much as possible for high readability: thus, the source is sometimes not PEP8 compliant, and has some..
occasionally questionable control flow.
