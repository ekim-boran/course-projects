{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Course1.W4 where

import Data.Bifunctor
import Data.List
import Data.List.Extra (maximumOn)
import Data.Maybe
import Data.Ord
import Data.Tuple.Extra hiding (first, second)
import Data.Vector qualified as V
import Prelude hiding (gcd, lcm)

q1 xs ys
  | null xs = replicate (length ys) Nothing
  | otherwise = search (V.fromList xs) <$> ys
  where
    search xs n = go 0 (length xs)
      where
        go start end | start == end = Nothing
        go start end
          | val == n = Just middle
          | val > n = go start middle
          | otherwise = go (middle + 1) end
          where
            middle = (start + end) `div` 2
            val = xs V.! middle

---q2

merge xss@(n1@(x, e1) : xs) yss@(n2@(y, e2) : ys)
  | x == y = (x, e1 + e2) : merge xs ys
  | x < y = n1 : merge xs yss
  | otherwise = n2 : merge xss ys
merge [] xs = xs
merge xs [] = xs

mergeSort xs = go (length xs) xs
  where
    go _ [] = []
    go _ [x] = [(x, 1)]
    go len xs = merge (go middle xs') (go rest ys')
      where
        (middle, rest) = (len `div` 2, len - middle)
        (xs', ys') = splitAt middle xs

q2 :: [Int] -> Int
q2 xs = fromEnum $ not $ null [x | (x, n) <- mergeSort xs, n > (length xs) `div` 2]

-- q3
-- TODO: implement mutable vector version
partition3 k (x : xs) =
  let (l, m, r) = partition3 k xs
   in case compare k x of
        EQ -> (l, x : m, r)
        LT -> (l, m, x : r)
        GT -> (x : l, m, r)
partition3 k _ = ([], [], [])

q3 = go []
  where
    go rest [x] = x : rest
    go rest (x : xs) =
      let (l, m, r) = partition3 x xs
       in go ((x : m) ++ go rest r) l
    go rest xs = rest

-- q4
-- buffer length to avoid recalculating each time

mergeQ4 (xss@(x : xs), !nx) ((y : ys), !ny)
  | x <= y = first (x :) $ mergeQ4 (xs, nx - 1) ((y : ys), ny)
  | otherwise = bimap (y :) (+ nx) $ mergeQ4 (xss, nx) (ys, ny - 1)
mergeQ4 ([], 0) (xs, _) = (xs, 0)
mergeQ4 (xs, _) ([], 0) = (xs, 0)

q4 :: [Int] -> Int
q4 xs = snd $ go (length xs) xs
  where
    go n [] = ([], 0)
    go 1 [x] = ([x], 0)
    go !len xs = f (go middle xs') (go rest ys')
      where
        (middle, rest) = (len `div` 2, len - middle)
        (xs', ys') = splitAt middle xs
        f (xs, !c1) (ys, !c2) = second (+ (c1 + c2)) $ mergeQ4 (xs, middle) (ys, rest)

-- q5

data ProjectedPoint = SegmentLeft {xCoord :: Int} | SegmentRight {xCoord :: Int} | Point {xCoord :: Int, index :: Int} deriving (Show)

q5 segments points = snd <$> sortOn (fst) (snd $ foldl' go (0, []) xs)
  where
    xs = sortOn xCoord $ concat [[SegmentLeft l, SegmentRight r] | (l, r) <- segments] ++ zipWith Point points ([0 ..])
    go (n, xs) (SegmentLeft i) = (n + 1, xs)
    go (n, xs) (SegmentRight i) = (n - 1, xs)
    go (n, xs) (Point _ index) = (n, (index, n) : xs)

--- q6

brute p points =
  if V.null points
    then maxBound
    else V.minimum $ (V.map (distance p) points)

distance (x1, y1) (x2, y2) = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

q6 points = go (V.fromList $ sortOn fst points)
  where
    go v | V.length v == 0 || V.length v == 1 = maxBound
    go v | V.length v == 2 = distance (v V.! 0) (v V.! 1)
    go vec = min dist distMiddle
      where
        (xs', ys') = V.splitAt (V.length vec `div` 2) vec
        dist = min (go xs') (go ys')
        distMiddle =
          if V.null ys'
            then maxBound
            else
              let (rx, ry) = V.last xs'
               in brute (rx, ry) $ V.takeWhile (\(x, y) -> x < rx + dist) ys' -- at most 16? points that match here

testQ1 = length $ mapMaybe id $ q1 [1, 5 .. 400000] [1 .. 100000]

testQ2 = q2 ([1 .. 10000] ++ (replicate 10000 1))

testQ3 = q3 $ reverse [1 .. 1000]

testQ4 = q4 ([1 .. 50000] ++ reverse [1 .. 50000])

testQ5 =
  uncurry q5 <$> [([(0, 5), (7, 10)], [1, 6, 11]), ([(-10, 10)], [-100, 100, 0]), (segments, points)]
  where
    segments = [(a, a + 40) | a <- [1 .. 50000]]
    points = [1 .. 50000]

testQ6 :: [Int]
testQ6 =
  q6
    <$> [ [(7, 7), (1, 100), (4, 8), (7, 7)],
          [(4, 4), (-2, -2), (-3, -4), (-1, 3), (2, 3), (-4, 0), (1, 1), (-1, -1), (3, -1), (-4, 2), (-2, 4)],
          points
        ]
  where
    points = fmap (\a -> (a, a * a)) [1 .. 50000]

-- >>> testQ6
-- [0,2,10]
