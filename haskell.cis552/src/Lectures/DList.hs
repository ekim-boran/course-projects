{-# LANGUAGE NoMonomorphismRestriction #-}

module Lectures.DList where

-- DLiSt https://www.cis.upenn.edu/~cis552/current/lectures/stub/01-intro/DList.html
type DList a = [a] -> [a]

singleton :: a -> DList a
singleton = (:)

cons :: a -> DList a -> DList a
cons x f = (x :) . f

append :: DList a -> DList a -> DList a
append = (.)

toList :: DList a -> [a]
toList f = f []

fromList :: [a] -> DList a
fromList xs = (xs ++)

empty :: DList a
empty = id

micro1 :: Char
micro1 = last (t 10000 "")
  where
    t :: Int -> [Char] -> [Char]
    t 0 l = l
    t n l = t (n - 1) (l ++ "s")

micro2 :: Char
micro2 = last (toList (t 10000 empty))
  where
    t :: Int -> DList Char -> DList Char
    t 0 l = l
    t n l = t (n - 1) (l `append` singleton 's')

-- >>> :set +s
-- (0.00 secs, 661,840 bytes)
--

-- >>> micro1
-- >>> micro2
-- 's'
-- 's'
--

reverseNaive = foldr (flip (++) . pure) []

-- >>> last $ reverseNaive [0..5000]
-- (0.00 secs, 661,816 bytes)
-- 0
-- (0.12 secs, 1,033,054,128 bytes)
--

reverseDList :: [a] -> [a]
reverseDList = toList . foldr (flip append . singleton) empty

-- >>> last $ reverseDList [0..10000]
-- (0.00 secs, 660,952 bytes)
-- 0
-- (0.01 secs, 3,670,136 bytes)
--

-- >>> last $ reverse [0..10000]
-- (0.00 secs, 660,952 bytes)
-- 0
-- (0.00 secs, 1,589,344 bytes)
--
