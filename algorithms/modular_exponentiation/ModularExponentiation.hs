{-
 - Various implementations of modular exponentiation with different algorithms
 - and performance characteristics.
 -}

{-# LANGUAGE BangPatterns #-}

module ModularExponentiation where

import qualified Data.List as List

{-
 - A recursive implementation of modular exponentiation. It'll happily overflow
 - the stack for large enough inputs.
 -}
modularPow :: Integral a => a -> a -> a -> a
modularPow _ 0 _ = 1
modularPow base exp' mod' =
	(base * modularPow base (exp' - 1) mod') `mod` mod'

{-
 - A tail-recursive implementation of modular exponentiation.
 -}
modularPow' :: Integral a => a -> a -> a -> a
modularPow' base exp' mod' =
	modularPow'Helper 1 0
	where
		modularPow'Helper !accumulator expCounter =
			if expCounter == exp'
				then accumulator
			else modularPow'Helper ((accumulator * base) `mod` mod') $
				expCounter + 1

{-
 - A strict, `foldl`-based implementation of modular exponentiation.
 -}
modularPow'' :: Integral a => a -> a -> a -> a
modularPow'' base exp' mod' =
	List.foldl' (\ acc _ -> (acc * base) `mod` mod') 1 [1..exp']
