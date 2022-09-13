{-# LANGUAGE NoImplicitPrelude #-}

-- | CSE 130: All about fold.
--
--  You may use any functions available to you without modifying the imports. You
--  can look at Cse130Prelude.hs for the complete list. In particular, you may
--  find zip/unzip useful.
--
--  Do not change the skeleton code! The point of this assignment is to figure out
--  how the functions can be written this way (using fold). You may only replace
--  the `error "TBD:..."` terms.
module Hw3 where

import Cse130Prelude

--------------------------------------------------------------------------------

-- | sqSum [x1, ... , xn] should return (x1^2 + ... + xn^2)
--
-- >>> sqSum []
-- 0
--
-- >>> sqSum [1,2,3,4]
-- 30
--
-- >>> sqSum [(-1), (-2), (-3), (-4)]
-- 30
sqSum :: [Int] -> Int
sqSum xs = foldl' f base xs
  where
    f acc elem = acc + (elem * elem)
    base = 0

--------------------------------------------------------------------------------

-- | `pipe [f1,...,fn] x` should return `f1(f2(...(fn x)))`
--
-- >>> pipe [] 3
-- 3
--
-- >>> pipe [(\x -> x+x), (\x -> x + 3)] 3
-- 12
--
-- >>> pipe [(\x -> x * 4), (\x -> x + x)] 3
-- 24
pipe :: [(a -> a)] -> (a -> a)
pipe fs = foldl' f base fs
  where
    f acc elem = acc . elem
    base = id

--------------------------------------------------------------------------------

-- | `sepConcat sep [s1,...,sn]` returns `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`
--
-- >>> sepConcat "---" []
-- ""
--
-- >>> sepConcat ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> sepConcat "#" ["a","b","c","d","e"]
-- "a#b#c#d#e"
sepConcat :: String -> [String] -> String
sepConcat _ [] = ""
sepConcat sep (x : xs) = foldl' f base l
  where
    f acc elem = acc ++ sep ++ elem
    base = x
    l = xs

intString :: Int -> String
intString = show

--------------------------------------------------------------------------------

-- | `stringOfList pp [x1,...,xn]` uses the element-wise printer `pp` to
--   convert the element-list into a string:
--
-- >>> stringOfList intString [1, 2, 3, 4, 5, 6]
-- "[1,2,3,4,5,6]"
--
-- >>> stringOfList (\x -> x) ["foo"]
-- "[foo]"
--
-- >>> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
-- "[[1,2,3],[4,5],[6],[]]"
stringOfList :: (a -> String) -> [a] -> String
stringOfList f xs = p $ sepConcat ", " $ map f xs where p x = "[" ++ x ++ "]"

--------------------------------------------------------------------------------

-- | `clone x n` returns a `[x,x,...,x]` containing `n` copies of `x`
--
-- >>> clone 3 5
-- [3,3,3,3,3]

-- >>> clone "foo" 2
-- ["foo","foo"]
--

clone :: a -> Int -> [a]
clone x n = take n $ repeat x
  where
    take 0 _ = []
    take _ [] = []
    take n (x : xs) = x : take (n - 1) xs
    repeat x = xs where xs = x : xs

type BigInt = [Int]

--------------------------------------------------------------------------------

-- | `padZero l1 l2` returns a pair (l1', l2') which are just the input lists,
--   padded with extra `0` on the left such that the lengths of `l1'` and `l2'`
--   are equal.
--
-- >>> padZero [9,9] [1,0,0,2]
-- ([0,0,9,9],[1,0,0,2])
--
-- >>> padZero [1,0,0,2] [9,9]
-- ([1,0,0,2],[0,0,9,9])
padZero :: BigInt -> BigInt -> (BigInt, BigInt)
padZero l1 l2
  | dif < 0 = (expand (-dif) l1, l2)
  | dif > 0 = (l1, expand dif l2)
  | otherwise = (l1, l2)
  where
    expand n num = clone 0 n ++ num
    dif = length l1 - length l2

--------------------------------------------------------------------------------

-- | `removeZero ds` strips out all leading `0` from the left-side of `ds`.
--
-- >>> removeZero [0,0,0,1,0,0,2]
-- [1,0,0,2]

-- >>> removeZero [9,9]
-- [9,9]
--
-- >>> removeZero [0,0,0,0]
-- []
--

removeZero :: BigInt -> BigInt
removeZero ds = dropWhile (== 0) ds
  where
    dropWhile _ [] = []
    dropWhile f (x : xs) = if f x then dropWhile f xs else (x : xs)

--------------------------------------------------------------------------------

-- | `bigAdd n1 n2` returns the `BigInt` representing the sum of `n1` and `n2`.
--
-- >>> bigAdd [9, 9] [1, 0, 0, 2]
-- [1,1,0,1]
--
-- >>> bigAdd [9, 9, 9, 9] [9, 9, 9]
-- [1,0,9,9,8]
bigAdd :: BigInt -> BigInt -> BigInt
bigAdd l1 l2 = removeZero (e : res)
  where
    (l1', l2') = padZero l1 l2
    (e, res) = foldl' f base args
    f (e, xs) (l, r) = (d, m : xs) where (d, m) = (l + r + e) `divMod` 10
    base = (0, [])
    args = zip (reverse l1') (reverse l2')

--------------------------------------------------------------------------------

-- | `mulByDigit i n` returns the result of multiplying
--   the digit `i` (between 0..9) with `BigInt` `n`.
--
-- >>> mulByDigit 9 [9,9,9,9]
-- [8,9,9,9,1]
mulByDigit :: Int -> BigInt -> BigInt
mulByDigit i n = r : res
  where
    (r, res) = foldr f (0, []) n
    f x (acc, rest) = let (d, m) = (i * x + acc) `divMod` 10 in (d, m : rest)

--------------------------------------------------------------------------------

-- | `bigMul n1 n2` returns the `BigInt` representing the product of `n1` and `n2`.
--
-- >>> bigMul [9,9,9,9] [9,9,9,9]
-- [9,9,9,8,0,0,0,1]
--
-- >>> bigMul [9,9,9,9,9] [9,9,9,9,9]
-- [9,9,9,9,8,0,0,0,0,1]

-- >>> bigMul [9, 9,9] [9,9,9]
-- [9,9,8,0,0,1]
--
bigMul :: BigInt -> BigInt -> BigInt
bigMul l1 l2 = res
  where
    (_, res) = foldl' f base args
    f (acc, rest) elem =
      (acc - 1, bigAdd (mulByDigit elem l2 ++ clone 0 acc) rest)
    base = (length l1 - 1, [])
    args = l1
