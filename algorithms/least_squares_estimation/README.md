# least-squares estimation
An implementation of the [least-squares estimation algorithm][overview], which computes the equation of a best-fit line
for any given data-set. The `least_squares_estimation` module contains the actual implementation in
`estimate_coefficients()`, while `demo` uses `matplotlib` to generate the following graphic, demonstrating use of the
algorithm:

![An image demonstrating the estimation of a best-fit line for a roughly exponential data-set.]
(least_squares_estimation.png)

To render the image live:
```bash
$ python demo.py
```

[overview]: http://en.wikipedia.org/wiki/Least_squares
