module Ex02 where

import Data.List
import Test.QuickCheck

-- implement the following functions, which meet some (but not all!) of the
-- properties of a correct sorting function

-- prop1 & 4, but not prop2 & 3 & 5



results = [(dodgySort1, [True, False, False, True, False]), (dodgySort2, [False, True, True, True, False]), (dodgySort3, [True, True, True, False, False])]

-- >>> props dodgySort1
-- [True,False,False,True,False]
--

dodgySort1 :: [Int] -> [Int]
dodgySort1 xs = xs

-- >>> props dodgySort2

-- *** Failed! Falsified (after 2 tests and 1 shrink):

-- [0]
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

-- *** Failed! Falsified (after 4 tests and 1 shrink):

-- [0]
-- [(),(),(),(),()]
--

-- prop2 & 3 & 4, but not prop1 & 5
dodgySort2 :: [Int] -> [Int]
dodgySort2 xs = insertionSort (xs ++ xs)

-- >>> props dodgySort3

-- prop1 & 2 & 3, but not prop4 & 5
dodgySort3 :: [Int] -> [Int]
dodgySort3 xs = take (length xs) [0 ..]

-- >>> props dodgySort4
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

-- *** Failed! Falsified (after 5 tests):

-- [-3,-3]
-- [(),(),(),(),()]
--

-- prop1 & 2 & 3 & 4, but not prop5
dodgySort4 :: [Int] -> [Int]
dodgySort4 xs = take (length xs) (insertionSort (nub xs) ++ [maxElem ..])
  where
    maxElem = maximum (0 : xs)

-- Properties of sorting function
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = length xs == length (sortFn xs)

sortProp2 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp2 sortFn xs = sortFn xs == sortFn (reverse xs)

sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True

sortProp4 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp4 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs = sortFn xs == insertionSort xs

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where
    insertSorted x [] = [x]
    insertSorted x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys
