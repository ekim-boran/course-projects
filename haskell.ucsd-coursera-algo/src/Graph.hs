module Graph where

import Control.Monad.ST
import Data.Array qualified as A
import Data.Array.Unboxed qualified as UA
import Data.Foldable
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.List (mapAccumL, (\\))
import Data.Tuple
import Data.Tuple.Extra
import Heap.BinomialHeap qualified as H

type Graph = A.Array Int [Int]

buildUG :: Int -> [(Int, Int)] -> Graph
buildUG n xs = A.accumArray (++) [] (1, n) $ concat [[(a, [b]), (b, [a])] | (a, b) <- xs]

buildG n xs = A.accumArray (++) [] (1, n) $ [(a, [b]) | (a, b) <- xs]

reverseGraph :: Graph -> Graph
reverseGraph g = A.accumArray (++) [] (A.bounds g) [(e, [s]) | (s, es) <- zip (A.indices g) (toList g), e <- es]

nodes = A.indices

edges = (A.!)

allEdges g = [(v, w) | v <- nodes g, w <- g A.! v]

data Tree a = Node
  { rootLabel :: a,
    subForest :: [Tree a]
  }
  deriving (Show)

cc :: Graph -> [Tree Int]
cc g = dfs g (nodes g)

dfs :: Graph -> [Int] -> [Tree Int]
dfs g xs = prune (map (generate) xs)
  where
    generate v = Node v (map (generate) (g A.! v))
    prune ts = snd $ chop S.empty ts
    chop set [] = (set, [])
    chop set (Node v ts : us) =
      if S.member v set
        then chop set us
        else
          let (set', ts') = chop (S.insert v set) ts
              (set'', us') = chop (S.insert v set') us
           in (set'', Node v (ts') : (us'))

--
bfs :: Graph -> [Int] -> [Tree Int]
bfs g xs = prune (generate <$> xs)
  where
    generate v = Node v (map (generate) (g A.! v))
    prune ts = snd $ chop' S.empty ts
    chop set (Node v ts) | S.member v set = (set, [])
    chop set (Node v ts) =
      let (set', as) = chop' (S.insert v set) ts
       in (set', [Node v (as)])
    chop' set ts = fmap concat $ mapAccumL (chop) set ts

bff g = bfs g (nodes g)

levels xs = [zip (pre [x]) [0 ..] | x <- xs]

bipartite g = not $ any (\(s, e) -> colors A.! s == colors A.! e) $ allEdges g
  where
    colors = A.array (A.bounds g) $ [(node, odd n) | xs <- levels $ bff g, (node, n) <- xs]

pre :: [Tree a] -> [a]
pre ts = go' ts []
  where
    go (Node a ts) = (a :) . go' ts
    go' ts = foldr (.) id $ map go ts

post :: [Tree a] -> [a]
post ts = go' ts []
  where
    go (Node a ts) = go' ts . (a :)
    go' ts = foldr (.) id $ map go ts

reachable :: Graph -> Int -> [Int]
reachable g v = pre (dfs g [v])

tabulate bnds xs = UA.array bnds (zipWith (flip (,)) [1 ..] (xs))

backEdges :: Graph -> [(Int, Int)]
backEdges g = [(a, b) | (a, b) <- allEdges g, postTable A.! a < postTable A.! b]
  where
    postTable = tabulate (A.bounds g) $ post $ dfs g (nodes g)

hasCycle :: Graph -> Bool
hasCycle = not . null . backEdges

topologicalSort :: Graph -> [Int]
topologicalSort = reverse . post . cc

scc :: Graph -> [Tree Int]
scc g = dfs g $ topologicalSort rg
  where
    rg = reverseGraph g

mapArray f arr = A.array (A.bounds arr) [(v, (f v (arr A.! v))) | v <- A.indices arr]

----
type WGraph = A.Array Int [(Int, Double)]

buildWGraph :: Int -> [(Int, Int, Double)] -> WGraph
buildWGraph n xs = A.accumArray (++) [] (1, n) [(s, [(e, w)]) | (s, e, w) <- xs]

dijkstra g start = go M.empty (H.singleton (0, start))
  where
    go map q =
      case H.minView q of
        Just ((_, node), q') | M.member node map -> go map q'
        Just ((w, node), q') -> go (M.insert node w map) (foldl' (flip H.insert) q' [(w + w', e) | (e, w') <- edges g node, M.notMember e map])
        Nothing -> map

bellmanFord g start = iterate go $ M.singleton start 0
  where
    go map = foldl' go' map (allEdges g)
    go' map (s, (e, w)) = case (M.lookup s map, M.lookup e map) of
      (Just w1, Just w2) -> if w2 <= (w + w1) then map else M.insert e (w + w1) map
      (Just w1, Nothing) -> M.insert e (w1 + w) map
      _ -> map

bf g start = (bellmanFord g start) !! (length g)

--
prim g = go [] S.empty (H.singleton (0, 1, 1))
  where
    go xs set q =
      case H.minView q of
        Just ((_, start, end), q') | S.member end set -> go xs set q'
        Just (x@(w, start, end), q') -> go (x : xs) (S.insert end set) (foldl' (flip H.insert) q' [(w', end, e) | (e, w') <- edges g end, S.notMember e set])
        Nothing -> xs
