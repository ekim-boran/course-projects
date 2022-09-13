module Course2.W1 where

import Control.Arrow
import Control.Exception (handle)
import Control.Monad (forM, forM_, replicateM, when)
import Data.Array
import Data.IntMap qualified as M
import Data.List (group)
import Data.List.Extra
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Queue.Bankers
import Queue.Class
import Queue.Class qualified as BQ
import RIO.Directory (listDirectory)
import RIO.FilePath (takeBaseName)
import System.IO
import Util

q1Runner = run' "./data/Course2/w1-1" hGetLine q1 (maybe "Success" show)

q1 xs = go [] (zip [1 ..] xs)
  where
    go stack ((n, x) : xs)
      | x `elem` ("{[(" :: String) = go ((n, x) : stack) xs
      | x `elem` (")]}" :: String) = case stack of
          ((i, y) : ys) -> if [y, x] `elem` ["()", "[]", "{}"] then go ys xs else Just n
          _ -> Just n
      | otherwise = go stack xs
    go [] [] = Nothing
    go ((n, x) : xs) [] = Just n

q2Runner = run' "./data/Course2/w1-2" parser q2 show
  where
    parser handle = do
      T.hGetLine handle
      tree <- T.hGetLine handle
      return (fmap readInt $ T.words tree)

q2 xs = go 0 (-1)
  where
    tree = accumArray (flip (:)) [] (-1, length xs) $ [(x, i) | (i, x) <- zip [0 ..] xs]
    go !n node = case tree ! node of
      [] -> n
      _ -> maximum $ [go (n + 1) i | i <- tree ! node]

--

q3Runner = run' "./data/Course2/w1-3" parser q3 toResult
  where
    parser handle = do
      (cap : n : _) <- (fmap readInt . T.words) <$> T.hGetLine handle
      xs <- replicateM n $ do
        (start : len : _) <- (fmap readInt . T.words) <$> T.hGetLine handle
        return (start, len)
      return (cap, xs)
    toResult = trim . unlines . fmap show

q3 (n, xs) = go (empty) xs
  where
    go deq ((start, e) : xs) =
      let deq' = dropWhileFront (<= start) deq
       in if lenBD deq' == n
            then (-1) : go deq' xs
            else case peekBack deq' of
              Nothing -> start : go (pushFront (start + e) deq') xs
              Just e' -> max start e' : go (pushBack (e + max start e') deq') xs
    go _ _ = []

-- >>> q3Runner
-- True

--

type MaxStack a = [(a, a)]

push a xss@((v, m) : xs) = (a, max m a) : xss
push a [] = [(a, a)]

pop [] = []
pop (x : xs) = xs

maxElem [] = 0
maxElem (x : xs) = snd x

data Action = Pop | Push Int | Max

q4 = fst . foldl' (\(xs, stack) ac -> first (xs ++) $ process ac stack) ([], [])
  where
    process Pop xs = ([], pop xs)
    process (Push x) xs = ([], push x xs)
    process Max xs = ([maxElem xs], xs)

q4Tests =
  q4
    <$> [ [Push 1, Push 7, Push 7, Max, Pop, Max],
          [Push 2, Push 3, Push 9, Push 7, Push 2, Max, Max, Max, Pop, Max],
          [Push 2, Push 1, Max, Pop, Max],
          ((Push <$> [1 .. 400000]) ++ (Push <$> [10 .. 100]) ++ [Max] ++ replicate (400090) Pop ++ [Max])
        ]

-- >>> q4Tests
-- [[7,7],[9,9,9,9],[2,2],[400000,1]]

popBackUnsafe d = fromMaybe undefined (popBack d)

q5 w xs = drop (w - 1) $ mapMaybe (fmap fst <$> peekBack) $ scanl (cons' w) (empty :: BankersDequeue (Int, Int)) (zip xs [0 ..])
  where
    cons' window d x@(v, i) =
      case peekBack d of
        Nothing -> (pushFront x d)
        (Just (v', i')) ->
          let d' = if i' + window <= i then popBackUnsafe d else d
           in (pushFront x $ dropWhileFront (< x) d')

-- >>>  q5' 4 [1..100]
-- [4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
