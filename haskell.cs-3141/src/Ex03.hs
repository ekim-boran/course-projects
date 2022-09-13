module Ex03 where

import           Data.List                      ( nub
                                                , sort
                                                )
import           Test.QuickCheck

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) =
  allTree (< v) l && allTree (>= v) r && isBST l && isBST r
 where
  allTree :: (Integer -> Bool) -> BinaryTree -> Bool
  allTree f (Branch v l r) = f v && allTree f l && allTree f r
  allTree f (Leaf        ) = True

--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r) | i < v     = Branch v (insert i l) r
                        | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf                       = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r)
  | i == j = let (x, l') = deleteRightmost l in Branch x l' (deleteAll i r)
  | i < j  = Branch j (deleteAll i l) r
  | i > j  = Branch j l (deleteAll i r)
 where
  deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
  deleteRightmost (Branch i l Leaf) = (i, l)
  deleteRightmost (Branch i l r) =
    let (x, r') = deleteRightmost r in (x, Branch i l r')

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
 where
  searchTrees' 0 = return Leaf
  searchTrees' n = do
    v <- (arbitrary :: Gen Integer)
    fmap (insert v) (searchTrees' $ n - 1)

----------------------

numberOfOccurences :: Integer -> BinaryTree -> Int
numberOfOccurences i Leaf = 0
numberOfOccurences i (Branch x l r) | i == x    = 1 + numberOfOccurences i r
                                    | i < x     = numberOfOccurences i l
                                    | otherwise = numberOfOccurences i r

mysteryProp :: Integer -> BinaryTree -> Int
mysteryProp = numberOfOccurences


-- >>> quickCheck prop_mysteryProp_1
-- +++ OK, passed 100 tests.
-- >>> quickCheck prop_mysteryProp_2
-- +++ OK, passed 100 tests.
--

prop_mysteryProp_1 integer = forAll searchTrees $ \tree ->
  mysteryProp integer (insert integer tree) > mysteryProp integer tree

prop_mysteryProp_2 integer = forAll searchTrees
  $ \tree -> mysteryProp integer (deleteAll integer tree) == 0

----------------------
mysterious :: BinaryTree -> [Integer]
mysterious = go [] where
  go xs Leaf           = xs
  go xs (Branch i l r) = go (i : go xs r) l


isSorted :: [Integer] -> Bool
isSorted (x : y : rest) = x <= y && isSorted (y : rest)
isSorted _              = True

-- >>> quickCheck prop_mysterious_1
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck prop_mysterious_2
-- +++ OK, passed 100 tests.
--



prop_mysterious_1 :: Integer -> Property
prop_mysterious_1 integer = forAll searchTrees
  $ \tree -> mysteryProp integer tree == (numInt $ mysterious tree)
  where numInt = length . filter (== integer)

prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious
----------------------


-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

astonishing :: [Integer] -> BinaryTree
astonishing [] = Leaf

astonishing xs = Branch r (astonishing l) (astonishing rs)
 where
  len           = length xs `div` 2
  (l, (r : rs)) = ((take len xs), drop len xs)


-- >>> quickCheck prop_astonishing_1
-- +++ OK, passed 100 tests.
--

-- >>> quickCheck prop_astonishing_2
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck prop_astonishing_3
-- +++ OK, passed 100 tests.
--

prop_astonishing_1 = forAll sortedListsWithoutDuplicates $ isBST . astonishing

prop_astonishing_2 =
  forAll sortedListsWithoutDuplicates $ isBalanced . astonishing

prop_astonishing_3 = forAll sortedListsWithoutDuplicates
  $ \integers -> mysterious (astonishing integers) == integers


isBalanced :: BinaryTree -> Bool
isBalanced Leaf           = True
isBalanced (Branch v l r) = and
  [abs (height l - height r) <= 1, isBalanced l, isBalanced r]
 where
  height Leaf           = 0
  height (Branch v l r) = 1 + max (height l) (height r)

