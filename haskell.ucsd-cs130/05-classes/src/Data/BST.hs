module Data.BST where

import qualified Data.List                     as L
import qualified Test.QuickCheck               as QC

-------------------------------------------------------------------------------
-- | BST data type 
-------------------------------------------------------------------------------

data BST a
  = Leaf                      -- ^ empty tree
  | Node a (BST a) (BST a)    -- ^ node with left and right subtrees
  deriving (Show)

-- | Binary-Search Ordering Invariant

isOrdered :: (Ord a) => BST a -> Bool
isOrdered Leaf = True
isOrdered (Node e l r) =
  forall l (< e) -- all elts in `l` are less    than `e`
    && forall r (e <) -- all elts in `r` are greater than `e` 
    && isOrdered l            -- left subtree `isOrdered`
    && isOrdered r            -- right subtree `isOrdered` 

forall :: BST a -> (a -> Bool) -> Bool
forall Leaf         _ = True
forall (Node e l r) p = p e && forall l p && forall r p

-------------------------------------------------------------------------------
-- | The empty BST
-------------------------------------------------------------------------------
empty :: BST a
empty = Leaf


-------------------------------------------------------------------------------
-- | Build a tree from a list
-------------------------------------------------------------------------------
build :: (Ord a) => [a] -> BST a
build = foldr add empty

-------------------------------------------------------------------------------
-- | Check membership in BST
-------------------------------------------------------------------------------
contains :: (Ord a) => a -> BST a -> Bool
contains _ Leaf = False
contains x (Node item l r) | item == x = True
                           | x < item  = contains x l
                           | otherwise = contains x r

t2 :: BST Int
t2 = Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))


-------------------------------------------------------------------------------
-- | In-order traversal (fold)
-------------------------------------------------------------------------------
fold :: (b -> a -> b) -> b -> BST a -> b
fold f b Leaf            = b
fold f b (Node item l r) = fold f (f (fold f b l) item) r

toList :: BST a -> [a]
toList = reverse . fold (\xs x -> x : xs) []

toString :: (Show a) => BST a -> String
toString t = "build " ++ show (toList t)

-- >>> toString t2
-- "build [5,10,20,30]"
--

-------------------------------------------------------------------------------
-- | Adding an element
-------------------------------------------------------------------------------
add :: (Ord a) => a -> BST a -> BST a
add x Leaf = Node x Leaf Leaf
add x t@(Node item l r) | item == x = t
                        | x < item  = Node item (add x l) r
                        | otherwise = Node item l (add x r)

-------------------------------------------------------------------------------
-- | Removing the minumum element
-------------------------------------------------------------------------------
removeMin :: (Ord a) => BST a -> (a, BST a)
removeMin Leaf = (undefined, Leaf)
removeMin (Node x Leaf r) = (x, r)
removeMin (Node x l r) = let (item, res) = removeMin l in (item, Node x res r)



-- >>> removeMin   (Node 23 Leaf Leaf)
-- (23,Leaf)
--

-- >>> remove 16 $ (Node 16 Leaf (Node 23 Leaf Leaf))
-- Node 23 Leaf Leaf
--


-------------------------------------------------------------------------------
-- | Removing an element
-------------------------------------------------------------------------------
remove :: (Ord a) => a -> BST a -> BST a
remove x Leaf = (Leaf)
remove x (Node item l r)
  | item == x = case r of
    Leaf -> l
    _    -> let (a, rest) = removeMin r in Node a l rest
  | x < item = Node item (remove x l) r
  | otherwise = Node item l (remove x r)

-------------------------------------------------------------------------------
-- | QuickCheck Properties
-------------------------------------------------------------------------------

--  Holds after `build`
prop_build :: [Int] -> Bool
prop_build xs = isOrdered (build xs)

--  Holds after `contains` and `build`
prop_contains_elt :: Int -> [Int] -> Bool
prop_contains_elt x xs = (x `elem` xs) == (contains x (build xs))

--  Holds after `contains` and `fold`
prop_contains_elts :: BST Int -> Bool
prop_contains_elts t = and [ contains x t | x <- toList t ]

-- Holds after `add`
prop_add_elt :: Int -> BST Int -> Bool
prop_add_elt elt t = contains elt (add elt t)

-- Holds after `add`
prop_add_elts_old :: Int -> BST Int -> Bool
prop_add_elts_old elt t = forall t (\x -> contains x t') where t' = add elt t

-- Holds after `add`
prop_add_isOrd :: Int -> BST Int -> Bool
prop_add_isOrd elt t = isOrdered (add elt t)

-- Holds after `add`: Fix this property
prop_multiset :: [Int] -> Bool
prop_multiset xs = toList (build xs) == (fmap head $ L.group $ L.sort xs)   -- <<<< TBD: you need to fix this property


-- Holds after `removeMin`
prop_remove_min :: BST Int -> Bool
prop_remove_min Leaf = True
prop_remove_min t    = contains x t && forall t' (\y -> x < y)
  where (x, t') = removeMin t

-- Holds after `remove`
prop_remove :: Int -> BST Int -> Bool
prop_remove elt t = not (contains elt t') where t' = remove elt t

-- Holds after `remove`
prop_remove_old :: Int -> BST Int -> Bool
prop_remove_old elt t = forall t (\x -> x == elt || contains x t')
  where t' = remove elt t

-- Holds after `remove`
prop_remove_isOrd :: Int -> BST Int -> Bool
prop_remove_isOrd elt t = isOrdered (remove elt t)

-------------------------------------------------------------------------------
-- | QuickCheck Instance
-------------------------------------------------------------------------------
quickCheck :: (QC.Testable prop) => prop -> IO ()
quickCheck = QC.quickCheck

instance (Ord a, QC.Arbitrary a) => QC.Arbitrary (BST a) where
  arbitrary = build <$> QC.arbitrary
