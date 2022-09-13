module S02_Custom
  () where

v1 :: (Integer, Float)
v1 = (4, 2.1)

v2 :: (Integer, Float, ())
v2 = (42, 666.0, ())

data MaybeInteger = MyNothing | MyJust Integer

v3 :: MaybeInteger
v3 = MyNothing

v4 :: MaybeInteger
v4 = MyJust 42

v5 :: Maybe Integer -- parametric polymorphism
v5 = Nothing

v6 :: Maybe Integer
v6 = Just 42
