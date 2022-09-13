{-# LANGUAGE BangPatterns #-}

-- | Assignment 1: implementing various small functions
module A01
  ( Day (..),
    nextWeekday,
    addTuple,
    productDot,
    maybeMap,
    maybeThen,
    Tree (..),
    sumTree,
    rightRotateTree,
    listSum,
    productSeq,
    setMem,
    setEquiv,
    setUnion,
    setIntersection,
    setDiff,
    setSymDiff,
    relMem,
    relEquiv,
    relComp,
    relTrans,
    relFull,
    fibs,
    primes,
    fuzzySeq,
    funComp,
    curry2,
    uncurry2,
    myFilter,
    myFilterMap,
    myFoldL,
    myRev,
  )
where

-- | TODO marker.
todo :: t
todo = error "todo"

myfoldl f = go
  where
    go !a (x : xs) = go (f a x) xs
    go a _ = a

-- | Days of week.
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Show, Enum)

-- | Returns the next weekday (excluding weekend, namely Saturday and Sunday).
nextWeekday :: Day -> Day
nextWeekday Monday = Tuesday
nextWeekday Tuesday = Wednesday
nextWeekday Wednesday = Thursday
nextWeekday Thursday = Friday
nextWeekday _ = Monday

-- | Add tuples of the 2-dimensional plane.
addTuple :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addTuple (a, b) (c, d) = (a + c, b + d)

-- | Dot-products two integer (list) vectors: https://en.wikipedia.org/wiki/Dot_product
-- |
-- | If the two vectors have different number of elements, you can return anything.
productDot :: [Integer] -> [Integer] -> Integer
productDot t1 t2 = listSum $ zipWith (*) t1 t2

-- | Maps the given value if it's Just.
maybeMap :: (Integer -> Integer) -> Maybe Integer -> Maybe Integer
maybeMap f (Just x) = Just $ f x
maybeMap f _ = Nothing

-- | If the given value is Just, map it with the given function; otherwise, the result is Nothing.
maybeThen :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
maybeThen (Just x) cont = cont x
maybeThen _ _ = Nothing

-- | Trees of integers.
data Tree = Leaf Integer | Branch Integer Tree Tree deriving (Eq, Show) -- Integer is value, Trees are left/right subtrees.

-- >>> "asd"
-- "asd"
--

-- | Sums all the integers in the given tree.
sumTree :: Tree -> Integer
sumTree (Leaf i) = i
sumTree (Branch i l r) = i + sumTree l + sumTree r

-- | Right-rotate the given tree. See https://en.wikipedia.org/wiki/Tree_rotation for more detail.
-- |
-- | Returns Nothing if there are not enough nodes.
rightRotateTree :: Tree -> Maybe Tree
rightRotateTree (Branch i (Branch j l r') r) = Just (Branch j l (Branch i r' r))
rightRotateTree _ = Nothing

-- | Maps the given list.
listMap f = go
  where
    go (x : xs) = f x : go xs
    go _ = []

-- | Sums all the integers in the given list.
listSum :: [Integer] -> Integer
listSum = myFoldL 0 (+)

-- | More compositional construction of sigma.
sumSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumSeq f from to = listSum (listMap f [from .. to])

-- | product of a sequence. See https://en.wikipedia.org/wiki/Multiplication#Product_of_a_sequence for more detail.
productSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
productSeq f from to = myFoldL 1 (*) (listMap f [from .. to])

-- | Returns if the given value is in the (list) set.
setMem :: Integer -> [Integer] -> Bool
setMem value (x : xs)
  | value == x = True
  | otherwise = setMem value xs
setMem _ _ = False

-- | Returns the two sets contain the same elements.
setEquiv :: [Integer] -> [Integer] -> Bool
setEquiv s1 s2 = all (`setMem` s1) s2 && all (`setMem` s2) s1

-- | Returns the set union.
setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion s1 s2 = filter (\s -> not $ setMem s s2) s1 ++ s2

-- | Returns the set intersection
setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection s1 s2 = filter (`setMem` s2) s1

-- | Returns the set diff, i.e., setDiff a b = $a - b$.
setDiff :: [Integer] -> [Integer] -> [Integer]
setDiff s1 s2 = filter (\s -> not $ setMem s s2) s1

-- | Returns the set symmetric diff.
setSymDiff :: [Integer] -> [Integer] -> [Integer]
setSymDiff s1 s2 = (s1 `setUnion` s2) `setDiff` (s1 `setIntersection` s2)

-- | Returns if the given pair is in the (list) relation.
relMem :: [(Integer, Integer)] -> Integer -> Integer -> Bool
relMem xs v1 v2 = not $ null $ [x | x <- xs, x == (v1, v2)]

-- | Returns the two relations contain the same elements.
relEquiv :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
relEquiv s1 s2 = all (uncurry (relMem s1)) s2 && all (uncurry (relMem s1)) s1

-- | Composes two relations, i.e., {(a,c) | exists b, (a,b) in r1 and (b,c) in r2}.
relComp :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
relComp r1 r2 = [(a, d) | (a, b) <- r1, (c, d) <- r2, b == c]

-- | Returns the transitive closure of the given relation: https://en.wikipedia.org/wiki/Transitive_closure
relTrans :: [(Integer, Integer)] -> [(Integer, Integer)]
relTrans rel = f $ f $ f rel
  where
    f rel =
      let x = (\r -> r ++ [(a, d) | (a, b) <- r, (c, d) <- r, b == c]) rel
       in if relEquiv rel x then rel else f x

-- | Returns the relation [0..n] * [0..n] = {(0,0), (0,1), ..., (0,n), (1,0), (1,1), ..., (1,n), ..., (n,n)}.
relFull :: Integer -> [(Integer, Integer)]
relFull n = [(a, b) | a <- [0 .. n], b <- [0 .. n]]

-- >>> take 10 fibs
-- [1,1,2,3,5,8,13,21,34,55]
--

-- | The Fibonacci sequence, starting with 0, 1, 1, 2, 3, ...
fibs :: [Integer]
fibs = fst <$> scanl (\(a, b) _ -> (b, a + b)) (0, 1) [0 ..]

-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]
--

-- | The primes, starting with 2, 3, 5, 7, ...
primes :: [Integer]
primes = primes' [2 ..]
  where
    primes' (x : xs) = x : primes' (diff xs (fmap (* x) [2 ..]))
    primes' [] = []
    diff (a : as) (b : bs)
      | a == b = diff as bs
      | a < b = a : diff as (b : bs)
      | otherwise = a : diff as bs
    diff _ _ = []

-- | The sequence of 1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 5, 4, 3, 2, 1, ...
fuzzySeq :: [Integer]
fuzzySeq = concatMap go [1 ..]
  where
    go n
      | n == 1 = [1]
      | otherwise = n : go (n - 1)

-- | Composes two functions, i.e., applies f1 and then f2 to the given argument
funComp :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer)
funComp f1 f2 x = f2 (f1 x)

-- | Transforms a function that gets single pair into a function that gets two arguments, i.e., curry2 f a1 a2 = f (a1, a2)
curry2 :: ((Integer, Integer) -> Integer) -> Integer -> Integer -> Integer
curry2 f a1 a2 = f (a1, a2)

-- | Transforms a function that gets two arguments into a function that gets single pair, i.e., uncurry2 f (a1, a2) = f a1 a2
uncurry2 :: (Integer -> Integer -> Integer) -> (Integer, Integer) -> Integer
uncurry2 f (a1, a2) = f a1 a2

-- | Filters the given list so that the the filter function returns True for the remaining elements.
myFilter :: (Integer -> Bool) -> [Integer] -> [Integer]
myFilter f (x : xs) = if f x then x : myFilter f xs else myFilter f xs
myFilter _ _ = []

-- | Maps the given list. If the map function returns Nothing, just drop it.
myFilterMap :: (Integer -> Maybe Integer) -> [Integer] -> [Integer]
myFilterMap f (x : xs)
  | (Just a) <- f x = a : myFilterMap f xs
  | otherwise = myFilterMap f xs
myFilterMap _ _ = []

-- | Folds the list from the left, i.e., myFoldL init f [l1, l2, ..., ln] = (f (f (f (f init l1) l2) ...) ln).
myFoldL :: Integer -> (Integer -> Integer -> Integer) -> [Integer] -> Integer
myFoldL init f = myfoldl f init

-- | Reverses the given list.
myRev :: [Integer] -> [Integer]
myRev = go []
  where
    go acc (x : xs) = go (x : acc) xs
    go acc _ = acc
