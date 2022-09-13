module Set.BST where

import Data.Foldable
import Data.List (partition)
import Data.Tuple.Extra
import Debug.Trace
import Heap.LeftistHeap (MinHeap (l))
import System.Random
import Test.QuickCheck
import Prelude hiding (lookup)

data BST e k a = E | Node !e !k !a (BST e k a) (BST e k a) deriving (Show)

class State t where
  bin :: k -> v -> BST t k v -> BST t k v -> BST t k v
  balance :: BST t k v -> BST t k v
  insert :: (Ord k) => k -> v -> BST t k v -> BST t k v
  insert key value E = bin key value E E
  insert key value (Node _ k v l r)
    | key == k = bin key value l r -- overwrite
    | key < k = balance $ bin k v (insert key value l) r
    | otherwise = balance $ bin k v l (insert key value r)

merge E x = x
merge l r = case minView r of
  Nothing -> l
  (Just ((k, v), r')) -> bin k v l r'

delete key E = E
delete key (Node _ k v l r)
  | key == k = merge l r
  | key < k = balance $ bin k v (delete key l) r
  | otherwise = balance $ bin k v l (delete key r)

instance State () where
  bin = Node ()
  balance = id

singleton k v = bin k v E E

empty E = True
empty _ = False

isBST :: (Ord k) => BST e k a -> Bool
isBST = go Nothing Nothing
  where
    go lower upper E = True
    go lower upper (Node _ k v l r) = ok lower upper k && go lower (Just k) l && go (Just k) upper r
    ok Nothing Nothing x = True
    ok Nothing (Just upper) x = x < upper
    ok (Just lower) Nothing x = lower < x
    ok (Just lower) (Just upper) x = lower < x && x < upper

lookup :: Ord t => t -> BST e t a -> Maybe a
lookup key E = Nothing
lookup key (Node x k v l r)
  | key == k = Just v
  | key < k = lookup key l
  | otherwise = lookup key r

preOrder :: BST e a b -> [(a, b)]
preOrder E = []
preOrder (Node _ k v l r) = (k, v) : preOrder l ++ preOrder r

inOrder :: BST e a b -> [(a, b)]
inOrder E = []
inOrder (Node _ k v l r) = inOrder l ++ [(k, v)] ++ inOrder r

postOrder :: BST e a b -> [(a, b)]
postOrder E = []
postOrder (Node _ k v l r) = postOrder l ++ postOrder r ++ [(k, v)]

fromInOrder :: [(k, v)] -> BST () k v
fromInOrder xs = go (length xs) xs
  where
    go 0 [] = E
    go 1 [(k, v)] = bin k v E E
    go n xs = bin k v (go middle l) (go (n - middle - 1) r)
      where
        middle = n `div` 2
        (l, (k, v) : r) = splitAt middle xs

fromAscList = fromDistinctAscList . go
  where
    go [] = []
    go [x] = [x]
    go ((k, v) : (k1, v1) : xs)
      | k == k1 = go ((k, v) : xs)
      | otherwise = (k, v) : go ((k1, v1) : xs)

fromDistinctAscList :: (State e) => [(k, a)] -> BST e k a
fromDistinctAscList xs =
  build const (length xs) xs
  where
    build c 0 xs' = c E xs'
    build c n xs' = build (buildR nr c) nl xs'
      where
        (nl, nr) = (n `div` 2, n - nl - 1)
    buildR n c l ((k, x) : ys) = build (buildB l k x c) n ys
    buildR _ _ _ [] = error "fromDistinctAscList buildR []"
    buildB l k x c r = c (bin k x l r)

fromList :: (Ord k, State e) => [(k, v)] -> BST e k v
fromList = foldl' insert' E
  where
    insert' map (k, v) = insert k v map

fromPreOrder :: (Ord k, State e) => [(k, a)] -> BST e k a
fromPreOrder [] = E
fromPreOrder ((k, v) : xs) = bin k v (fromPreOrder l) (fromPreOrder r)
  where
    (l, r) = span ((< k) . fst) xs

inorderProp :: BST e Int Int -> Bool
inorderProp tree = inOrder tree == (inOrder . fromInOrder . inOrder) tree

isBSTProp :: BST e Int Int -> Bool
isBSTProp = isBST

getMin E = Nothing
getMin (Node _ k v E r) = Just (k, v)
getMin (Node _ k v l r) = getMin l

minView E = Nothing
minView (Node _ k v E r) = case minView r of
  Nothing -> Just ((k, v), E)
  (Just ((k1, v1), r')) -> Just ((k, v), balance $ bin k1 v1 E r')
minView (Node _ k v l r) = second (\l' -> balance $ bin k v l' r) <$> minView l

maxView E = Nothing
maxView (Node _ k v l E) = case maxView l of
  Nothing -> Just ((k, v), E)
  (Just ((k1, v1), l')) -> Just ((k, v), balance $ bin k1 v1 l' E)
maxView (Node _ k v l r) = second (balance . bin k v l) <$> maxView r

rotateLeft E = E
rotateLeft x@(Node _ k v l E) = x
rotateLeft (Node _ k v l (Node _ k1 v1 l1 r1)) = bin k1 v1 (bin k v l l1) r1

rotateRight E = E
rotateRight x@(Node _ k v E r) = x
rotateRight x@(Node _ k v (Node _ k1 v1 l1 r1) r) = bin k1 v1 l1 (bin k v r r1)

minViewProp :: (State e, Eq a, Eq b) => BST e a b -> Bool
minViewProp t = inOrder t == go t
  where
    go t = case minView t of
      Nothing -> []
      (Just (k, t')) -> k : go t'

construct = isBST $ fromDistinctAscList @() $ dup <$> [0 .. 100]
  where
    dup a = (a, a)

deleteProp :: BST () Int Int -> Bool
deleteProp t = empty $ foldl' (\x (k, v) -> delete k x) t (inOrder t)

t = quickCheck (deleteProp .&&. isBSTProp @() .&&. (inorderProp @()) .&&. (minViewProp @() @Int @Int))

testT = delete 12 $ Node () 3 (-2) (Node () 0 1 E E) E

-- >>> t
-- +++ OK, passed 100 tests.
--

---- >>> t
---- +++ OK, passed 100 tests.
----

instance (Ord a, Arbitrary a, Arbitrary v, State e) => Arbitrary (BST e a v) where
  arbitrary = fromList <$> listOf arbitrary
