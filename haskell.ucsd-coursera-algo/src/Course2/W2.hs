module Course2.W2 where

import Control.Monad (replicateM)
import Control.Monad.ST
import Data.List.Extra
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V hiding (generate, length)
import Data.Vector.Mutable qualified as V
import DisjointUnion
import Heap.BinomialHeap qualified as P
import Util

q1Runner = run' "./data/Course2/w2-1" parser q1 print'
  where
    parser handle = do
      T.hGetLine handle
      tree <- T.hGetLine handle
      return (fmap readInt $ T.words tree)
    print' xs = (trim $ unlines $ show (length xs) : fmap (\(a, b) -> show a ++ " " ++ show b) xs)

bubbledown vec index = do
  xs <- sequence [(,i) <$> V.read vec i | i <- [index, (index + 1) * 2, (index + 1) * 2 - 1], i < V.length vec]
  let i = snd $ minimumOn fst xs
  if i == index
    then return []
    else do
      V.swap vec index i
      rest <- bubbledown vec i
      return $ (index, i) : rest

q1 xs = runST $ do
  v <- V.unsafeThaw $ V.fromList xs
  xs <- traverse (bubbledown v) (reverse [0 .. ((V.length v) `div` 2)])
  return $ concat xs

-- >>> q1Runner

q2Runner = run' "./data/Course2/w2-2" parser q2 print'
  where
    parser handle = do
      (t : _) <- (fmap readInt . T.words) <$> T.hGetLine handle
      tree <- (fmap readInt . T.words) <$> T.hGetLine handle
      return (t, tree)
    print' = (trim . unlines . fmap (\(a, b) -> show a ++ " " ++ show b))

q2 (nthread, xs) = go xs $ P.fromList [(0, i) | i <- [0 .. (nthread - 1)]]
  where
    go (x : xs) (P.minView -> Just ((time, tid), p')) = (tid, time) : go xs (P.insert (time + x, tid) p')
    go _ _ = []

-- >>> q2Runner

--- q3

q3Runner = run' "./data/Course2/w2-3" q3Parser q3 (trim . unlines . fmap show)

q3Parser handle = do
  (_ : nqueries : _) <- readInts <$> T.hGetLine handle
  init <- readInts <$> T.hGetLine handle
  actions <- replicateM nqueries $ do
    (f : s : _) <- readInts <$> T.hGetLine handle
    return (f, s)
  return (init, actions)

q3 (init, xs) = runST $ mkDu (0 : init) >>= go (maximum init) xs
  where
    go m ((f, s) : xs) du' = do
      r <- connect f s du'
      ((max r m) :) <$> go (max r m) xs du'
    go m _ _ = return []

testQ3 =
  q3 ([10, 0, 5, 0, 3, 3, 6, 6], [(6, 6), (6, 5), (5, 4), (4, 3)])
