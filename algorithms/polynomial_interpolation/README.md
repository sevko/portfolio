# Newtonian polynomial interpolation
This project implements Newtonian polynomial interpolation, and used `matplotlib` to generate several animations of the
interpolation of points on a polynomial curve as the set of known points increased in size:

![First polynomial interpolation animation.][interpolation_animation_1]
![Second polynomial interpolation animation.][interpolation_animation_2]
![Third polynomial interpolation animation: a sin curve is used.][interpolation_animation_3]

`polynomial_interpolation.py` implements Newton's method in `interpolate_polynomial_value()`, while `animation` handles
the `matplotlib` graphics. Sample usage:

```python
poly_points = [
	(-3.0, -5.0),
	(-2.0, -1.1),
	(2.0, 1.9),
	(3.0, 4.8)
]
interpolate_polynomial_value(poly_points, -2.5) # approx. -2.69375
```

Running `animation.py` will simply execute `polynomial_interpolation`'s inbuilt unit test.

[interpolation_animation_1]: animation/interpolation0.gif
[interpolation_animation_2]: animation/interpolation1.gif
[interpolation_animation_3]: animation/interpolation2.gif
