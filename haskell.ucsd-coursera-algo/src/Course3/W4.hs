{-# LANGUAGE NoMonomorphismRestriction #-}

module Course3.W4 where

import Graph
import Course3.TestGraphs
import Data.Array qualified as A
import Data.Bifunctor (first)
import Data.Foldable
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.Tuple (swap)
import Heap.BinomialHeap qualified as H

q1 = dijkstra

negativeCycle g = M.keysSet $ M.filter id $ M.intersectionWith (>) (ford !! (length g)) (ford !! (2 * length g))
  where
    ford = bellmanFord g 1

q2 = not . S.null . negativeCycle

data D = Reachable Double | Negative | NotReachable deriving (Show)

q3 g = xs `M.union` (M.fromList ((,NotReachable) <$> (nodes g)))
  where
    ford = bellmanFord g 1
    xs = M.intersectionWith (\a b -> if a == b then Reachable a else Negative) (ford !! (length g)) (ford !! (2 * length g))

--


testQ1 = (uncurry q1) <$> [(wg1, 1), (wg2, 1), (wg3, 3)]

testQ2 = q2 wg5

testQ3 = q3 <$> [wg5, wg6]
