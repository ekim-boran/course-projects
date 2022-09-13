module S04_Function
  () where

addOne :: Integer -> Integer
addOne n = n + 1

addFuzzy :: Integer -> Integer
addFuzzy n | even n = n + 1
addFuzzy n          = n + 2

addFuzzyIf :: Integer -> Integer
addFuzzyIf n = if even n then n + 1 else n + 2

_ = addFuzzyIf 2

addFuzzy2 :: Integer -> Integer -> Integer
addFuzzy2 n m | even n = m + 1
addFuzzy2 n m          = m + 2

_ = addFuzzy2 2 3
-- addFuzzy2         : Integer -> (Integer -> Integer)
-- (addFuzzy2 2)     : Integer -> Integer
-- ((addFuzzy2 2) 3) : Integer

addFuzzy22 :: Integer -> Integer
addFuzzy22 m = m + 1

addFuzzyPair :: (Integer, Integer) -> Integer
addFuzzyPair (n, m) | even n = m + 1
addFuzzyPair (n, m)          = m + 2

_ = addFuzzyPair (2, 3)

addFuzzyRec :: Integer -> Integer
addFuzzyRec n | even n = n + 1 -- even's type is Integer -> Bool
addFuzzyRec n          = addFuzzyRec (n + 1)

addValue :: Integer
addValue = (+) 1 2 -- the same with 1 + 2

addFuzzyRec1 :: Integer -> Integer
addFuzzyRec1 n | even n = n + 1
addFuzzyRec1 n          = addFuzzyRec2 (n + 2)

addFuzzyRec2 :: Integer -> Integer
addFuzzyRec2 n | even n = n + 2
addFuzzyRec2 n          = addFuzzyRec1 (n + 2)

funNameless :: Integer -> Integer
funNameless i = i ^ 2

listMap :: (Integer -> Integer) -> [Integer] -> [Integer]
listMap f []        = []
listMap f (hd : tl) = (f hd) : listMap f tl

sigma :: (Integer -> Integer) -> Integer -> Integer -> Integer
sigma f from to = go from 0
 where
  go i s | i > to = s
  go i s          = go (i + 1) (s + f i)
sumFuzzy n = sigma (\i -> i ^ 2) 0 n
_ = sumFuzzy 10

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs10 :: [Integer]
fibs10 = take 10 fibs

-- How to return the 4th element in a 5 elements list? Or how to return the 6th element in a 7 elements list? How to return second to last (the element before the last one)? -> exercise
