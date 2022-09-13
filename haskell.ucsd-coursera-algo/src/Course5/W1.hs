module Course5.W1 where

import Control.Arrow
import Control.Monad (forM, replicateM)
import Data.IntMap qualified as M
import Data.List (foldl', nub, sortBy, sortOn, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Util (check, parse, readInt, readInts, run, run')

type Graph = M.IntMap (M.IntMap Int)

type Edge = ((Int, Int), Int)

buildG xs = foldl' modifyEdge M.empty xs

modifyEdge :: Graph -> Edge -> Graph
modifyEdge g ((s, e), n) = M.alter (Just . f) s g
  where
    f Nothing = go M.empty
    f (Just m) = go m
    go m =
      case M.lookup e m of
        Nothing -> M.insert e n m
        (Just x) | x + n == 0 -> M.delete e m -- zero
        (Just x) -> M.insert e (x + n) m

edges :: Graph -> Int -> [Edge]
edges graph source = case M.lookup source graph of
  Nothing -> []
  (Just m) ->
    [((source, e), n) | (e, n) <- reverse $ M.assocs m]

residual source target graph = case path graph source target of
  [] -> Nothing
  xs -> Just (flow, foldl' addFlow graph flow)
    where
      minFlow = minimum $ (fmap snd xs ++ [maxBound])
      flow = [(a, minFlow) | (a, n) <- xs]
      addFlow g ((s, e), n) = modifyEdge (modifyEdge g ((e, s), n)) ((s, e), -n)

path :: Graph -> Int -> Int -> [Edge]
path graph source target = bfs S.empty [(source, [])]
  where
    bfs visited ((node, path) : xs)
      | S.member node visited = bfs visited xs
      | node == target = path
      | otherwise = bfs (S.insert node visited) (xs ++ [(t, a : path) | a@((s, t), n) <- edges graph node])
    bfs _ [] = []

maxflow source target graph = unfoldr (residual source target) graph

parseQ1 handle = do
  (target : n : _) <- (fmap readInt . T.words) <$> T.hGetLine handle
  g <- replicateM n $ do
    (s : t : n : _) <- readInts <$> T.hGetLine handle
    return ((s, t), n)
  return (target, buildG g)

q1 = totalFlow . uncurry (maxflow 1)
  where
    totalFlow = sum . fmap (snd . head)

-- >>> q1Runner
-- True

q1Runner = run' "./data/Course5/w1-1" parseQ1 q1 (show)

testGraph = [((1, 2), 5), ((1, 3), 7), ((3, 2), 3), ((2, 4), 10), ((3, 4), 5)]

--

parseQ2 handle = do
  (a : b : _) <- (fmap readInt . T.words) <$> T.hGetLine handle
  g <- forM [0 .. (a - 1)] $ \a' -> do
    xs <- (fmap readInt . T.words) <$> T.hGetLine handle
    let es = [((a', t), 1) | (t, x) <- zip [a ..] xs, x == 1]
    return (es)
  let source = minBound
      target = maxBound
      xs = (concat g) ++ [((source, x), 1) | x <- [0 .. (a - 1)]] ++ [((x, target), 1) | x <- [a .. (a + b - 1)]]
  return $ (a, source, target, buildG xs)

q2Runner = run' "./data/Course5/w1-2" parseQ2 q2 (show)

q2 (n, s, t, g) = unwords $ show <$> [maybe (-1) (\a -> a - n + 1) $ lookup i as | i <- [0 .. (n - 1)]]
  where
    as = [(a, b) | ((a, b), _) <- flow, a /= s, b /= t, ((b, a), 1) `notElem` flow]
    flow = concat $ maxflow s t g

-- >>> q2Runner
-- error 04 "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100"
