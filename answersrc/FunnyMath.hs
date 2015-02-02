{-|
Module: FunnyMath
Description: Buggy implementations of math, for illustrating testing.
Copyright: Franklin Chen, 2015
License: BSD3
Maintainer: franklinchen@franklinchen.com

Just to illustrate HSpec and QuickCheck use.
-}
module FunnyMath where

-- | Do addition.
add :: Int -> Int -> Int
add x y = x + y

-- | Do multiplication.
--
-- Some example-based tests:
--
-- >>> multiply 2 2
-- 4
--
-- >>> multiply 2 3
-- 6
--
-- A sample QuickCheck generative test:
--
-- prop> multiply n 0 == 0
multiply :: Int -> Int -> Int
multiply x y = x * y
