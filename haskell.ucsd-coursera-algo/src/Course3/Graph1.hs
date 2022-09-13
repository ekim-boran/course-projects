module Course3.Graph1 where

import Data.Array qualified as A
import Data.Foldable
import Data.Foldable qualified as A
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.Tuple
import Data.Tuple.Extra
import Heap.BinomialHeap qualified as H

type Graph = A.Array Int [Int]

mkUndirectedGraph :: Int -> [(Int, Int)] -> Graph
mkUndirectedGraph n xs = A.accumArray (++) [] (1, n) $ concat [[(a, [b]), (b, [a])] | (a, b) <- xs]

mkDirectedGraph n xs = A.accumArray (++) [] (1, n) $ [(a, [b]) | (a, b) <- xs]

reverseGraph :: Graph -> Graph
reverseGraph g = A.accumArray (++) [] (A.bounds g) [(e, [s]) | (s, es) <- zip (A.indices g) (A.toList g), e <- es]

nodes = A.indices

edges = (A.!)

cc g = bfs' g (nodes g)

reachable g start = undefined

---

doNothing _ x = x

bfs' g = helper S.empty
  where
    helper s (node : nodes) = visited : helper visited (filter (`S.notMember` visited) nodes)
      where
        visited = go s [node]
    helper _ [] = []
    go visited (x : xs) | S.member x visited = go visited xs
    go visited (x : xs) =
      go (S.insert x visited) ((edges g x) ++ xs)
    go v _ = v

dfs g = explore doNothing doNothing [] g

explore pre post state g = helper S.empty
  where
    helper s (node : nodes) =
      let (visited, state') = go s state [Right node]
       in state' : helper visited (filter (`S.notMember` visited) nodes)
    helper _ [] = []

    go visited state (Right x : xs) =
      go (S.insert x visited) (pre x state) ((Right <$> filter (`S.notMember` visited) (edges g x)) ++ [Left x] ++ xs)
    go visited state (Left x : xs) = go visited (post x state) xs
    go v state _ = (v, state)

bfs f state g = helper S.empty
  where
    helper s (node : nodes) =
      let (visited, state') = go s state [(node, 0)]
       in state' : helper visited (filter (`S.notMember` visited) nodes)
    helper s _ = []
    go visited state ((x, c) : xs) =
      go (S.insert x visited) (f (x, c) state) (xs ++ ((,c + 1) <$> filter (`S.notMember` visited) (edges g x)))
    go visited state _ = (visited, state)

type WeightedGraph = A.Array Int [(Int, Double)]

allEdges g = [(n, e, w) | n <- nodes g, (e, w) <- edges g n]

mkWeightedGraph :: Int -> [(Int, Int, Double)] -> WeightedGraph
mkWeightedGraph n xs = A.accumArray (++) [] (1, n) (fmap f xs)
  where
    f (a, b, c) = (a, [(b, c)])

dijkstra g start = go M.empty (H.singleton (0, start))
  where
    go map (H.minView -> (Just ((w, node), q'))) =
      if M.notMember node map then go (M.insert node w map) (foldl' (flip H.insert) q' newNodes) else go map q'
      where
        newNodes = fmap (first (w +) . swap) (edges g node)
    go map _ = map

bellmanFord graph start = bellmanFord' graph start (M.singleton start (0))

bellmanFord' graph start weights = maps !! numIter
  where
    numIter = length (nodes graph)
    maps = iterate (flip (foldl' f) (allEdges graph)) weights
    f map (s, e, w) = case (M.lookup s map, M.lookup e map) of
      (Just x, Nothing) -> M.insert e (x + w) map
      (Just x, Just y) -> if y < x + w then map else M.insert e (x + w) map
      (_, _) -> map

