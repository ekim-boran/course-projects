{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module HW5.Sequence where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, foldM, forM, guard, liftM, liftM2)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.QuickCheck.Function

class (Monad l, Foldable l) => Sequence l where
  -- construction
  nil :: l a
  single :: a -> l a
  append :: l a -> l a -> l a

  -- position based operations
  first :: l a -> Maybe a
  final :: l a -> Maybe a
  index :: Int -> l a -> Maybe a
  insert :: (Show a) => Int -> a -> l a -> Maybe (l a)

pairs :: Sequence l => l a -> l b -> l (a, b)
pairs xs ys = [(x, y) | x <- xs, y <- ys]

instance Sequence [] where
  nil = []
  single x = [x]
  append = (++)
  first l = guard (not (null l)) >> return (head l)
  final l = guard (not (null l)) >> return (last l)
  index n l = guard (0 <= n && n < length l) >> return (l !! n)
  insert n x l = guard (0 <= n && n < length l) >> return (before ++ x : after)
    where
      (before, after) = splitAt n l

data AVL a
  = Empty
  | Single a
  | Branch
      Int -- cached number of elements
      Int -- cached height
      (AVL a) -- left child
      (AVL a) -- right child
  deriving (Show)

seq1 :: AVL Int
seq1 =
  Branch
    4
    2
    (Branch 2 1 (Single 7) (Single 3))
    (Branch 2 1 (Single 4) (Single 5))

instance Sequence AVL where
  nil = Empty
  single = Single
  append = avlAppend
  first = avlFirst
  final = avlFinal
  index = avlIndex
  insert = avlInsert

testPairs :: Test
testPairs =
  "pairs" ~: toList (pairs seq1 seq1)
    ~=? [ (7, 7),
          (7, 3),
          (7, 4),
          (7, 5),
          (3, 7),
          (3, 3),
          (3, 4),
          (3, 5),
          (4, 7),
          (4, 3),
          (4, 4),
          (4, 5),
          (5, 7),
          (5, 3),
          (5, 4),
          (5, 5)
        ]

-- (a) first and final

-- | access the first element of the sequence, if there is one.
avlFirst :: AVL a -> Maybe a
avlFirst Empty = Nothing
avlFirst (Single a) = Just a
avlFirst (Branch _ _ l r) = avlFirst l <|> avlFirst r

-- | access the last element of the list, if there is one (similar to above)
avlFinal :: AVL a -> Maybe a
avlFinal Empty = Nothing
avlFinal (Single a) = Just a
avlFinal (Branch _ _ l r) = avlFinal r <|> avlFinal l

testFirst :: Test
testFirst =
  TestList
    [ "first" ~: first seq1 ~=? Just 7,
      "final" ~: final seq1 ~=? Just 5
    ]

-- >>> runTestTT testFirst
-- (0.00 secs, 672,176 bytes)
-- <BLANKLINE>
-- Cases: 2  Tried: 0  Errors: 0  Failures: 0
-- Cases: 2  Tried: 1  Errors: 0  Failures: 0
--
-- Cases: 2  Tried: 2  Errors: 0  Failures: 0
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
-- (0.01 secs, 848,024 bytes)
--

-- (b) Reducing sequences

instance Foldable AVL where
  -- The default definition of the length function looks something like this:
  length Empty = 0
  length (Single _) = 1
  length (Branch n _ _ _) = n

  -- Override this definition with an optimized version that is O(1)

  -- Finish the `foldr` definition below so that it is O(n) (Hint: see HW2)
  foldr f b Empty = b
  foldr f b (Single x) = f x b
  foldr f b (Branch _ _ xs ys) = foldr f (foldr f b ys) xs

toList :: Sequence l => l a -> [a]
toList = foldr (:) []

instance Eq a => Eq (AVL a) where
  l1 == l2 = toList l1 == toList l2

testFoldable :: Test
testFoldable =
  TestList
    [ "length" ~: length seq1 ~?= 4,
      "toList" ~: toList seq1 ~?= [7, 3, 4, 5],
      "sum" ~: sum seq1 ~?= 19
    ]

-- >>> runTestTT testFoldable
-- (0.00 secs, 673,040 bytes)
-- <BLANKLINE>
-- Cases: 3  Tried: 0  Errors: 0  Failures: 0
-- Cases: 3  Tried: 1  Errors: 0  Failures: 0
-- Cases: 3  Tried: 2  Errors: 0  Failures: 0
--
-- Cases: 3  Tried: 3  Errors: 0  Failures: 0
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}
-- (0.01 secs, 888,544 bytes)
--

-- (c)  Indexing

avlIndex :: Int -> AVL a -> Maybe a
avlIndex i (Single a) | i == 0 = Just a
avlIndex i (Branch n h l r) | i < n = if i < length l then avlIndex i l else avlIndex (i - length l) r
avlIndex _ Empty = Nothing

testAvlIndex =
  TestList
    [ "index 0" ~: avlIndex 0 seq1 ~?= Just 7,
      "index 1" ~: avlIndex 1 seq1 ~?= Just 3,
      "index 2" ~: avlIndex 2 seq1 ~?= Just 4,
      "index 3" ~: avlIndex 3 seq1 ~?= Just 5
    ]

-- >>> runTestTT testAvlIndex

-- (d) Insert
rotateRight (Branch n h (Branch n1 h1 l1 r1) r) = branch l1 (branch r1 r)
rotateRight x = x

rotateLeft (Branch n h l (Branch n1 h1 l1 r1)) = branch (branch l l1) r1
rotateLeft x = x

rebalance :: AVL e -> AVL e
rebalance t@(Branch n h l r)
  | bf t == 2 && bf l == 1 = rotateRight t -- left left
  | bf t == -2 && bf r == -1 = rotateLeft t -- right right
  | bf t == 2 = rotateRight (Branch 0 0 (rotateLeft l) r) --left right
  | bf t == -2 = rotateLeft (Branch 0 0 l (rotateRight r))
  | otherwise = t
rebalance x = x

branch :: AVL a -> AVL a -> AVL a
branch x y = Branch (length x + length y) (1 + max (height x) (height y)) x y

height :: AVL a -> Int
height Empty = 0
height (Single x) = 0
height (Branch _ k s1 s2) = k

avlInsert :: (Show a) => Int -> a -> AVL a -> Maybe (AVL a)
avlInsert i e Empty | i == 0 = Just (Single e)
avlInsert i e (Single a) | i == 0 = Just $ branch (Single e) (Single a)
avlInsert i e (Single a) | i == 1 = Just $ branch (Single a) (Single e)
avlInsert i e (Branch n h s1 s2)
  | i < length s1 = rebalance . flip branch s2 <$> avlInsert i e s1
  | i <= n = rebalance . branch s1 <$> avlInsert (i - length s1) e s2
avlInsert i e x = error (show i ++ show x)

testAvlInsert :: Test
testAvlInsert =
  TestList
    [ "insert 0 " ~: toList <$> insert 0 1 seq1 ~?= Just [1, 7, 3, 4, 5],
      "insert 1 " ~: toList <$> insert 1 1 seq1 ~?= Just [7, 1, 3, 4, 5],
      "insert 2 " ~: toList <$> insert 2 1 seq1 ~?= Just [7, 3, 1, 4, 5],
      "insert 3 " ~: toList <$> insert 3 1 seq1 ~?= Just [7, 3, 4, 1, 5],
      "insert 4 " ~: toList <$> insert 4 1 seq1 ~?= Just [7, 3, 4, 5, 1]
    ]

-- >>> runTestTT testAvlInsert
-- Cases: 5  Tried: 0  Errors: 0  Failures: 0
-- Cases: 5  Tried: 1  Errors: 0  Failures: 0
-- Cases: 5  Tried: 2  Errors: 0  Failures: 0
-- Cases: 5  Tried: 3  Errors: 0  Failures: 0
-- Cases: 5  Tried: 4  Errors: 0  Failures: 0
--
-- Cases: 5  Tried: 5  Errors: 0  Failures: 0
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}
--
-- Cases: 5  Tried: 5  Errors: 0  Failures: 0
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}
-- (0.01 secs, 999,688 bytes)
--

-- (e) Testing with quickcheck

instance (Show a, Arbitrary a) => Arbitrary (AVL a) where
  arbitrary = do
    pairs <- zip [0 ..] <$> listOf arbitrary
    return $ foldl (\t (a, b) -> fromMaybe Empty $ avlInsert a b t) Empty pairs

-- >>> sample (arbitrary :: (Gen(AVL Int)))

--shrink _ = undefined

prop_length :: AVL Int -> Bool
prop_length xs = count xs == count xs
  where
    count Empty = 0
    count (Single x) = 1
    count (Branch j _ l r) = count l + count r

prop_height :: AVL Int -> Bool
prop_height xs = count xs == count xs
  where
    count Empty = 0
    count (Single x) = 0
    count (Branch _ k l r) = 1 + max (height l) (height r)

prop_balanced :: AVL Int -> Bool
prop_balanced Empty = True
prop_balanced (Single x) = True
prop_balanced t@(Branch _ _ l r) =
  bf t >= -1 && bf t <= 1 && prop_balanced l && prop_balanced r

-- the balance factor
bf :: AVL a -> Int
bf (Branch _ _ l r) = height l - height r
bf (Single _) = 0
bf Empty = 0

prop_AVL :: AVL Int -> Property
prop_AVL x =
  counterexample "length" (prop_length x)
    .&&. counterexample "height" (prop_height x)
    .&&. counterexample "balanced" (prop_balanced x)

-- >>> quickCheck prop_AVL
-- +++ OK, passed 100 tests.
--

-- (f) append

avlAppend :: AVL a -> AVL a -> AVL a
avlAppend Empty x = x
avlAppend x Empty = x
avlAppend x y | bf (branch x y) >= -1 && bf (branch x y) <= 1 = branch x y
avlAppend x@(Branch n h l r) y | bf (branch x y) >= 2 = rebalance $ branch l (avlAppend r y)
avlAppend x y@(Branch n h l r) | bf (branch x y) <= -2 = rebalance $ branch (avlAppend x l) r

prop_append :: AVL Int -> AVL Int -> Bool
prop_append l1 l2 = toList (l1 `append` l2) == toList l1 ++ toList l2

prop_append_AVL :: AVL Int -> AVL Int -> Property
prop_append_AVL l1 l2 = prop_AVL (avlAppend l1 l2)

-- >>> quickCheck prop_append_AVL
-- +++ OK, passed 100 tests.

-- Functors and Monads (at last!) (g)

instance Functor AVL where
  fmap f Empty = Empty
  fmap f (Single a) = Single (f a)
  fmap f (Branch n h l r) = Branch n h (f <$> l) (f <$> r)

instance Applicative AVL where
  pure = Single
  (<*>) = ap -- this function is defined in terms of bind

instance Monad AVL where
  return = Single
  Empty >>= _ = Empty
  (Single a) >>= f = f a
  (Branch n h l r) >>= f = (l >>= f) `append` (r >>= f)

prop_FMapId :: (Eq (f a), Functor f) => f a -> Bool
prop_FMapId x = fmap id x == id x

prop_FMapComp :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
prop_FMapComp (Fun _ f) (Fun _ g) x =
  fmap (f . g) x == (fmap f . fmap g) x

prop_LeftUnit :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
prop_LeftUnit x (Fun _ f) =
  (return x >>= f) == f x

prop_RightUnit :: (Eq (m b), Monad m) => m b -> Bool
prop_RightUnit m =
  (m >>= return) == m

prop_Assoc ::
  (Eq (m c), Monad m) =>
  m a ->
  Fun a (m b) ->
  Fun b (m c) ->
  Bool
prop_Assoc m (Fun _ f) (Fun _ g) =
  ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)

prop_FunctorMonad :: (Eq (m b), Monad m) => m a -> Fun a b -> Bool
prop_FunctorMonad x (Fun _ f) = fmap f x == (x >>= return . f)

qc1 :: IO ()
qc1 =
  quickCheck
    (prop_FMapId :: AVL Int -> Bool)

qc2 :: IO ()
qc2 =
  quickCheck
    (prop_FMapComp :: Fun Int Int -> Fun Int Int -> AVL Int -> Bool)

qc3 :: IO ()
qc3 =
  quickCheck
    (prop_LeftUnit :: Int -> Fun Int (AVL Int) -> Bool)

qc4 :: IO ()
qc4 = quickCheck (prop_RightUnit :: AVL Int -> Bool)

qc5 :: IO ()
qc5 =
  quickCheck
    (prop_Assoc :: AVL Int -> Fun Int (AVL Int) -> Fun Int (AVL Int) -> Bool)

qc6 :: IO ()
qc6 =
  quickCheck
    (prop_FunctorMonad :: AVL Int -> Fun Int (AVL Int) -> Bool)

qc7 :: IO ()
qc7 = undefined

qc8 :: IO ()
qc8 = undefined

qc9 :: IO ()
qc9 = undefined

qc10 :: IO ()
qc10 = quickCheck prop_AVL_functor
  where
    prop_AVL_functor :: Fun Int Int -> AVL Int -> Property
    prop_AVL_functor (Fun _ f) x = prop_AVL (fmap f x)

qc11 :: IO ()
qc11 = quickCheck prop_AVL_return
  where
    prop_AVL_return :: Int -> Property
    prop_AVL_return x = prop_AVL (return x)

qc12 :: IO ()
qc12 = quickCheck prop_AVL_bind
  where
    prop_AVL_bind :: AVL Int -> Fun Int (AVL Int) -> Property
    prop_AVL_bind x (Fun _ k) = prop_AVL (x >>= k)

qcAVL :: IO ()
qcAVL = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >> qc10 >> qc11 >> qc12

-- >>>   qcAVL

main :: IO ()
main = do
  runTestTT $
    TestList
      [ testPairs,
        testFirst,
        testFoldable,
        testAvlIndex,
        testAvlInsert
      ]
  quickCheck prop_AVL
  quickCheck prop_append
  quickCheck prop_append_AVL
  qcAVL
