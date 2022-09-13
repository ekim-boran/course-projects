module Queue.BatchedQueue where

import Queue.Class

data BQ a = BQ [a] [a]

check :: [a] -> [a] -> BQ a
check [] xs = BQ (reverse xs) []
check xs ys = BQ xs ys

instance Queue BQ where
  empty = BQ [] []
  isEmpty (BQ xs ys) = null xs
  peekFront (BQ (x : xs) r) = Just $ x
  peekFront _ = Nothing
  popFront (BQ (x : xs) ys) = Just $ check xs ys
  popFront _ = Nothing
  pushBack a (BQ xs ys) = check xs (a : ys)
