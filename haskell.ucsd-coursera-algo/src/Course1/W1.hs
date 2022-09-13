module Course1.W1 where

import Data.List (foldl')
import Data.List.Extra
import Data.Tuple.Extra
import Test.Hspec
import Test.QuickCheck

-- question : Maximum Pairwise Product

prop xs =
  naive xs == (uncurry (*) <$> answer1 xs)
    && naive xs == (uncurry (*) <$> answer2 xs)

naive :: [Int] -> Maybe Int
naive xs | length xs <= 1 = Nothing
naive xs = (Just . product . take 2 . reverse . sort) xs

answer1 :: [Int] -> Maybe (Int, Int)
answer1 xs | length xs <= 1 = Nothing
answer1 xs = Just $ foldl' go (minBound, minBound) xs
  where
    go (f, s) x1
      | s > x1 = (f, s)
      | f > x1 = (f, x1)
      | otherwise = (x1, f)

-- https://stackoverflow.com/questions/9889679/find-second-largest-number-in-array-at-most-nlog%E2%82%82n%E2%88%922-comparisons
answer2 :: [Int] -> Maybe (Int, Int)
answer2 xs | length xs <= 1 = Nothing
answer2 xs = Just $ second maximum $ head $ (go []) $ zip xs (repeat [])
  where
    go r (x : y : rest) = go (compare x y : r) rest
    go (x : r) [y] = go (compare x y : r) []
    go r _ | length r == 1 = r
    go r _ = go [] r -- next round
    compare (a, as) (b, bs)
      | a < b = (b, a : bs)
      | otherwise = (a, b : as)

 
