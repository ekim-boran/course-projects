module S02_Nat
  ( Nat
  ) where

data Nat = O | S Nat

v0 :: Nat
v0 = O

v1 :: Nat
v1 = S O

v2 :: Nat
v2 = S (S O)

v3 :: Nat
v3 = S (S (S O))
