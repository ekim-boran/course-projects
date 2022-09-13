module S01_Gcd
  ( gcdSlow
  , gcdFast
  , gcdFaster
  ) where

gcdSlow :: Integer -> Integer -> Integer
gcdSlow a b =
  let xs = [ x | x <- [1 .. (min a b)], a `rem` x == 0, b `rem` x == 0 ]
  in  last xs

gcdFast :: Integer -> Integer -> Integer
gcdFast 0 b          = b
gcdFast a 0          = a
gcdFast a b | a >= b = gcdFast (a - b) b
gcdFast a b          = gcdFast a (b - a)

gcdFaster :: Integer -> Integer -> Integer
gcdFaster 0 b          = b
gcdFaster a 0          = a
gcdFaster a b | a >= b = gcdFaster (a `rem` b) b
gcdFaster a b          = gcdFaster a (b `rem` a)
