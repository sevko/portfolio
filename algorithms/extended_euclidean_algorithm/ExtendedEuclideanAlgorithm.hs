{-
 - An implementation of the extended Euclidean algorithm.
 -}

module ExtendedEuclideanAlgorithm where

{-
 - An implementation of the Extended Euclidean Algorithm, which, given two
 - integers `a` and `b`, computes the coefficients `x` and `y` that satisfy the
 - Bezout identity: `a * x + b * y = gcd(a, b)`. Both positive and negative
 - values are accepted.
 -}
bezoutCoefficients :: Int -> Int -> (Int, Int)

{-
 - Given `b = 0`, we know that `gcd(a, b)` is `abs(a)`, and also that, in the
 - Bezout identity (`a * x + b * y = gcd(a, b)`), `x` must equal 1 if `a` is
 - positive and -1 if `a` is negative. `b` can be anything, so we'll just
 - choose 0.
 -}
bezoutCoefficients a 0 = (if a < 0 then -1 else 1, 0)
bezoutCoefficients a b = let
	(quotient, remainder) = divMod a b
	(firstCoef, secondCoef) = bezoutCoefficients b remainder
	in (secondCoef, firstCoef - quotient * secondCoef)
