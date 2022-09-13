{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Heap.BinomialHeap where

import Data.Foldable hiding (toList)

type BinomialHeap a = [Node a]

data Node a = Node
  { rank :: !Int,
    item :: !a,
    children :: BinomialHeap a
  }
  deriving (Show)

empty = []

singleton item = [Node 0 item []]

-- merge nodes that have same rank
mergeSameRank h1@(Node rank1 item1 c1) h2@(Node rank2 item2 c2)
  | rank1 /= rank2 = error "they should be same rank "
  | otherwise =
      if item1 < item2
        then Node (rank1 + 1) item1 (h2 : c1)
        else Node (rank1 + 1) item2 (h1 : c2)

insertNode :: Ord a => [Node a] -> Node a -> [Node a]
insertNode [] n = [n]
insertNode h@(f : rest) n
  | rank n < rank f = n : h
  | rank n == rank f = insertNode rest (mergeSameRank n f)
  | otherwise = error "not a valid operation"

merge :: Ord a => [Node a] -> [Node a] -> [Node a]
merge h [] = h
merge [] h = h
merge (x : xs) (y : ys)
  | rank x < rank y = x : merge xs (y : ys)
  | rank x > rank y = y : merge (x : xs) ys
  | otherwise = insertNode (merge xs ys) (mergeSameRank x y)

insert item = merge (singleton item)

minNode (x : xs) = case minNode xs of
  Nothing -> Just (x, [])
  (Just (x', xs')) -> if item x < item x' then Just (x, xs) else Just (x', x : xs')
minNode _ = Nothing

pop h = case minNode h of
  Nothing -> Nothing
  (Just (x, xs)) -> Just $ merge xs (reverse $ children x)

peek :: (Ord a) => BinomialHeap a -> Maybe a
peek = fmap (item . fst) . minNode

minView h = case minNode h of
  Nothing -> Nothing
  (Just (x, xs)) -> Just (item x, merge xs (reverse $ children x))

toList :: (Ord a) => BinomialHeap a -> [a]
toList = go
  where
    go (minView -> (Just (i, r))) = i : go r
    go _ = []

fromList = foldl' (flip insert) empty

testHeap = last $ toList $ foldl' (flip insert) empty [0 .. 400000]

testHeap2 = fmap rank $ foldl' (flip insert) empty [0 .. 100]

-- >>> :set +s
-- >>> testHeap
-- (0.00 secs, 706,272 bytes)
-- 400000
-- (4.71 secs, 3,645,050,640 bytes)
--
