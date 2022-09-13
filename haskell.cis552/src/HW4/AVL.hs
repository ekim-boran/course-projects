{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HW4.AVL
  ( Set (..),
    AVL (..),
    avlEmpty,
    avlElements,
    avlMember,
    avlInsert,
    avlDelete,
    t1,
    t2,
    t3,
    bad1,
    bad2,
    bad3,
    main,
    rebalance,
    height,
    bf,
    setProperties,
    prop_empty,
    prop_elements,
    prop_insert1,
    prop_insert2,
    prop_delete1,
    prop_delete2,
    prop_delete3,
    avlProperties,
    prop_bst,
    prop_ht,
    prop_balance,
  )
where

import Data.List (nub)
import Test.QuickCheck hiding (elements)
import Prelude hiding (zipWith, zipWith3)

class Set s where
  empty :: s a
  member :: Ord a => a -> s a -> Bool
  insert :: Ord a => a -> s a -> s a
  elements :: s a -> [a]
  delete :: Ord a => a -> s a -> s a

instance Set AVL where
  empty = avlEmpty
  member = avlMember
  insert = avlInsert
  elements = avlElements
  delete = avlDelete

-- 1
prop_empty :: forall s. (Set s) => Bool
prop_empty = null (elements (empty :: s Int))

-- no duplicates
prop_elements :: AVL Int -> Bool
prop_elements t = length (nub l) == length l
  where
    l = elements t

prop_insert1 :: Int -> AVL Int -> Bool
prop_insert1 x t = member x (insert x t)

prop_insert2 :: Int -> Int -> AVL Int -> Property
prop_insert2 x y t = x /= y ==> member y t == member y (insert x t)

prop_delete1 :: AVL Int -> Bool
prop_delete1 t = empty == foldr delete t (elements t)

prop_delete2 :: AVL Int -> Int -> Bool
prop_delete2 t a = not $ member a (delete a (insert a t))

-- delete removes elements
prop_delete3 :: AVL Int -> Int -> Property
prop_delete3 t x = member x t ==> not $ member x (delete x t)

setProperties :: Property
setProperties =
  counterexample "empty" (prop_empty @AVL)
    .&&. counterexample "elts" prop_elements
    .&&. counterexample "insert1" prop_insert1
    .&&. counterexample "insert2" prop_insert2
    .&&. counterexample "delete1" prop_delete1
    .&&. counterexample "delete2" prop_delete2
    .&&. counterexample "delete3" prop_delete3

-- 2

data AVL e
  = E -- empty tree
  | N -- non-empty tree
      Int -- cached height of the tree
      (AVL e) -- left subtree
      e -- value
      (AVL e) -- right subtree
  deriving (Show)

-- | Access the height of the tree
height :: AVL e -> Int
height E = 0
height (N h _ _ _) = h

-- | Calculate the balance factor of a node
bf :: AVL e -> Int
bf E = 0
bf (N _ l _ r) = height l - height r

-- | The tree is a binary search tree
prop_bst :: AVL Int -> Bool
prop_bst E = True
prop_bst (N _ l x r) = all (x >) (elements l) && all (x <) (elements r) && prop_bst l && prop_bst r

-- | The height at each node is correctly calculated.
prop_ht :: AVL Int -> Bool
prop_ht E = True
prop_ht (N h l _ r) = h - 1 == max (height l) (height r) && prop_ht l && prop_ht r

-- | The balance factor at each node is between -1 and +1.
prop_balance :: AVL Int -> Bool
prop_balance E = True
prop_balance t@(N h l _ r) = bf t <= 1 && bf t >= -1 && prop_ht l && prop_ht r

avlProperties :: Property
avlProperties =
  counterexample "bst" prop_bst
    .&&. counterexample "height" prop_ht
    .&&. counterexample "balance" prop_balance

instance (Eq a) => Eq (AVL a) where
  (==) l r = elements l == elements r

instance (Ord e, Arbitrary e) => Arbitrary (AVL e) where
  arbitrary = foldr insert empty <$> listOf arbitrary

--shrink = undefined

-- 3

-- | an empty AVL tree
avlEmpty :: AVL e
avlEmpty = E

-- | list the elements in the tree, in order
avlElements :: AVL e -> [e]
avlElements E = []
avlElements (N _ l x r) = avlElements l ++ [x] ++ avlElements r

-- | Determine if an element is contained within the tree
avlMember :: Ord e => e -> AVL e -> Bool
avlMember x (N _ l y r)
  | x < y = avlMember x l
  | x > y = avlMember x r
  | otherwise = True
avlMember _ _ = False

-- 4

g = sample (arbitrary @(AVL Int))

-- >>> g
-- (0.00 secs, 672,192 bytes)

t1 :: AVL Int
t1 = undefined

t2 :: AVL Int
t2 = undefined

t3 :: AVL Int
t3 = undefined

bad1 :: AVL Int
bad1 = undefined

bad2 :: AVL Int
bad2 = undefined

bad3 :: AVL Int
bad3 = undefined

create l x r = N (1 + max (height l) (height r)) l x r

-- 5
rotateRight (N h (N h1 l1 x1 r1) x r) = create l1 x1 (create r1 x r)
rotateRight x = x

rotateLeft (N h l x (N h1 l1 x1 r1)) = create (create l x l1) x1 r1
rotateLeft x = x

-- | Rotate an AVL tree
rebalance :: (Ord e) => AVL e -> AVL e
rebalance E = E
rebalance t@(N h l x r)
  | bf t == 2 && bf l == 1 = rotateRight t -- left left
  | bf t == -2 && bf r == -1 = rotateLeft t -- right right
  | bf t == 2 = rotateRight (N 0 (rotateLeft l) x r) --left right
  | bf t == -2 = rotateLeft (N 0 l x (rotateRight r))
  | otherwise = t

-- 6

-- | Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert e E = N 1 E e E
avlInsert e t@(N _ l x r)
  | e < x = rebalance $ create (avlInsert e l) x r
  | e > x = rebalance $ create l x (avlInsert e r)
  | otherwise = t

-- 7

removeMin (N _ E x r) = Just (x, r)
removeMin (N _ l x r) = (\(a, l') -> (a, rebalance $ create l' x r)) <$> removeMin l
removeMin E = Nothing

-- | Delete the provided element from the tree
avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete e E = E
avlDelete e t@(N _ l x r)
  | e < x = rebalance $ create (avlDelete e l) x r
  | e > x = rebalance $ create l x (avlDelete e r)
  | otherwise = maybe l (\(x', r') -> rebalance $ create l x' r') $ removeMin r

main :: IO ()
main = return ()

-- >>> quickCheck avlProperties
-- (0.00 secs, 672,192 bytes)
-- +++ OK, passed 100 tests.
-- (0.16 secs, 80,179,568 bytes)
--
-- >>> quickCheck setProperties
-- (0.00 secs, 672,192 bytes)
-- +++ OK, passed 100 tests; 393 discarded.
-- (1.28 secs, 798,179,072 bytes)
--
