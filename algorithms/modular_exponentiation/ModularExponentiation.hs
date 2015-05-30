{-
 - Various implementations of modular exponentiation with different algorithms
 - and performance characteristics.
 -}

{-# LANGUAGE BangPatterns #-}

module ModularExponentiation where

import qualified Data.List as List
import qualified Data.Bits as Bits

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

{-
 - An implementation of binary exponentiation, or exponentiation by squaring.
 -}
modularPow''' :: (Bits.Bits e, Integral e, Integral a) => a -> e -> a -> a
modularPow''' base exp' mod' =
	modularPow'''Helper base exp' 1
	where
		modularPow'''Helper _ 0 result = result
		modularPow'''Helper currBase currExp result =
			let
				newResult = if odd currExp
					then (result * currBase) `mod` mod'
					else result
				newExp = Bits.shiftR currExp 1
				newBase = (currBase * currBase) `mod` mod'
			in modularPow'''Helper newBase newExp newResult
