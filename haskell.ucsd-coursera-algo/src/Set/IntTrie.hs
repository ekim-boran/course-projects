{-# LANGUAGE AllowAmbiguousTypes #-}

module Set.IntTrie where

import Data.Bits

data IntTree a
  = Empty
  | Leaf Int a
  | Branch Int Int (IntTree a) (IntTree a) -- prefix and mask

insert :: Int -> t -> IntTree t -> IntTree t
insert k x Empty = Leaf k x
insert k x t@(Leaf k' x') = if k == k' then Leaf k x else join k (Leaf k x) k' t
insert k x t@(Branch p m l r)
  | match k p m = if zero k m then Branch p m (insert k x l) r else Branch p m l (insert k x r)
  | otherwise = join k (Leaf k x) p t

join :: Int -> IntTree a -> Int -> IntTree a -> IntTree a
join p1 t1 p2 t2 = if zero p1 m then Branch p m t1 t2 else Branch p m t2 t1
  where
    (p, m) = lcp p1 p2

lcp :: Int -> Int -> (Int, Int)
lcp p1 p2 = (p, m)
  where
    m = bit (highestBit (p1 `xor` p2))
    p = mask p1 m

highestBit 0 = 0
highestBit n = 1 + highestBit (shiftR n 1)

mask x m = x .&. complement (m - 1)

zero x m = x .&. (shiftR m 1) == 0

match k p m = (mask k m) == p