module S02_Point
  ( P
  , P2
  ) where

data P = Point1D Integer | Point2D Integer Integer

v1 :: P
v1 = Point1D 42

v2 :: P
v2 = Point2D 42 666

data P2 = A Integer | B Integer

v3 :: P2
v3 = A 42

v4 :: P2
v4 = B 42

-- v3 and v4 are different values.
