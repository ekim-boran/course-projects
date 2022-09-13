{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Heap.LeftistHeap where

import Data.Foldable hiding (toList)

data MinHeap a = L | B {item :: a, h :: !Int, l :: !(MinHeap a), r :: !(MinHeap a)}

-- rank is minimum distance to  a leaf node

rank L = 0
rank (B _ h _ _) = h

mkNode a l r
  | rank l < rank r = B a (rank l + 1) r l
  | otherwise = B a (rank r + 1) l r

empty = L

merge h L = h
merge L h = h
merge h1@(B i1 _ l1 r1) h2@(B i2 _ l2 r2)
  | i1 < i2 = mkNode i1 l1 (merge r1 h2)
  | otherwise = mkNode i2 l2 (merge h1 r2)

insert item heap = mkNode item L L `merge` heap

peek (B {..}) = Just item
peek _ = Nothing

pop L = L
pop (B {..}) = merge l r

minView L = Nothing
minView (B {..}) = Just (item, merge l r)

toList :: (Ord a) => MinHeap a -> [a]
toList = go
  where
    go (minView -> (Just (i, r))) = i : go r
    go _ = []

testHeapL = last $ toList $ foldl' (flip insert) empty [0 .. 400000]

testHeap2 = rank $ foldl' (flip insert) empty [0 .. 400000]

-- >>> :set +s
-- >>> testHeapL
-- Some flags have not been recognized: +s
-- 400000
 
