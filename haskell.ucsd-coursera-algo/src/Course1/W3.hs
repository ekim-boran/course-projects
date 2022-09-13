{-# LANGUAGE TupleSections #-}

module Course1.W3 where

import Data.List
import Data.Ord
import Data.Tuple.Extra
import Prelude hiding (gcd, lcm)

q1 target = sum . fmap snd . f target . sortOn Down
  where
    f target (item : items) = let (count, m) = target `divMod` item in (item, count) : f m items
    g _ _ = []

q2 target = sum . fmap fst . f target . sortOn (Down . uncurry (/))
  where
    f target (x@(value, weight) : xs)
      | target >= weight = x : f (target - weight) xs
      | otherwise = [(target * (value / weight), target)]
    f 0 _ = []
    f target [] = []

q3 :: Int -> Int -> [Int] -> Int
q3 distance range = maybe (-1) length . (go 0)
  where
    go n stops | n + range > distance = Just []
    go n stops =
      case span (< (n + range)) stops of
        ([], _) -> Nothing
        (xs, ys) -> let x = last xs in (x :) <$> go x ys

q4 as bs = sum $ zipWith (*) (sort as) (sort bs)

q5 = go . sortOn snd
  where
    go [] = []
    go ((s, e) : xs) = e : go (filter (not . between e) xs)
    between m (a, b) = m >= a && m <= b

q6 n = unfoldr go (1, n)
  where
    go (cur, left)
      | left == 0 = Nothing
      | left > 2 * cur = Just (cur, ((cur + 1), (left - cur)))
      | otherwise = Just (left, (cur, 0))

q7 xs = concat $ reverse $ sortBy f $ show <$> xs
  where
    f [] b = GT
    f _ [] = LT
    f (a : as) (b : bs)
      | a == b = f as bs
      | otherwise = compare a b

---  tests

testQ1 = q1 2 [10, 5, 1]

testQ2 = [q2 50 [(60, 20), (100, 50), (120, 30)], q2 3 [(500, 30)]] == [180.0, 50.0]

testQ3 = [q3 950 400 [200, 375, 550, 750], q3 10 3 [1, 2, 5, 9], q3 200 250 [100, 150]]

testQ4 = [q4 [1, 3, -5] [-2, 4, 1]]

testQ5 = [q5 [(1, 3), (2, 5), (3, 6)], q5 [(4, 7), (1, 3), (2, 5), (5, 6)]]

testQ6 = q6 <$> [1 .. 10]

testQ7 = q7 <$> [[21, 2], [9, 4, 6, 1, 9], [23, 39, 92]]
