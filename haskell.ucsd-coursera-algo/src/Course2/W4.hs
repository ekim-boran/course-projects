module Course2.W4 where

import Control.Monad (replicateM)
import Data.List.Extra
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Tuple.Extra
import Data.Vector qualified as V
import Set.Splay qualified as S
import Util

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

preOrder Leaf = id
preOrder (Node a l r) = (a :) . (preOrder l) . (preOrder r)

inorder Leaf = id
inorder (Node a l r) = (inorder l) . (a :) . (inorder r)

postOrder Leaf = id
postOrder (Node a l r) = (postOrder l) . (postOrder r) . (a :)

isBST' Leaf = True
isBST' t = sorted $ inorder t []
  where
    sorted xs = and $ zipWith (<=) xs (tail xs)

isBST :: (Ord a) => Tree a -> Bool
isBST = go Nothing Nothing
  where
    go lower upper Leaf = True
    go lower upper (Node k l r) =
      compare (>) k lower && compare (<) k upper
        && go lower (Just k) l
        && go (Just k) upper r
    compare f k = maybe (True) (f k)

--

q1Runner = run' "./data/Course2/w4-1" q1Parser q1 (trim . unlines . fmap (trim . unwords . fmap show))

q1Parser handle = do
  (n : _) <- readInts <$> T.hGetLine handle
  nodes <- replicateM n $ do
    (k : l : r : _) <- readInts <$> T.hGetLine handle
    return (k, l, r)
  return $ if null nodes then Leaf else (toBin (V.fromList nodes) 0)
  where
    toBin vec (-1) = Leaf
    toBin vec n = Node k (toBin vec l) (toBin vec r)
      where
        (k, l, r) = vec V.! n

q1 tree = (\f -> f tree []) <$> [inorder, preOrder, postOrder]

-- >>> q2Runner
-- True

q2Runner = run' "./data/Course2/w4-2" q1Parser q2 (\b -> if b then "CORRECT" else "INCORRECT")

q2 = isBST

-- q3

q3Runner = run' "./data/Course2/w4-3" q1Parser q3 (\b -> if b then "CORRECT" else "INCORRECT")

q3 = isBSTHard

isBSTHard :: (Ord a) => Tree a -> Bool
isBSTHard = go Nothing Nothing
  where
    go lower upper Leaf = True
    go lower upper (Node k l r) =
      compare (>=) k lower && compare (<) k upper
        && go lower (Just k) l
        && go (Just k) upper r
    compare f k = maybe (True) (f k)

-- >>> q3Runner
-- True

q4Runner = run' "./data/Course2/w4-4" q4Parser q4 (trim . unlines)

data Op = Add Int | Delete Int | Lookup Int | Sum Int Int deriving (Show)

q4Parser handle = do
  (n : _) <- readInts <$> T.hGetLine handle
  nodes <- replicateM n $ do
    (op : a : xs) <- T.words <$> T.hGetLine handle
    return $ case op of
      "+" -> Add (readInt $ a)
      "-" -> Delete (readInt $ a)
      "?" -> Lookup (readInt $ a)
      "s" -> Sum (readInt $ a) (readInt (head xs))
  return nodes

q4 :: [Op] -> ([String])
q4 = reverse . snd3 . foldl' go (0, [], S.empty)
  where
    get sum n = (sum + n) `mod` 1000000001
    go (sum', xs, s) (Add x) = (sum', xs, S.insert (get sum' x) () s)
    go (sum', xs, s) (Delete x) = (sum', xs, S.delete (get sum' x) s)
    go (sum', xs, s) (Lookup x) = case S.find (get sum' x) s of
      (Nothing, s) -> (sum', "Not found" : xs, s)
      (Just _, s) -> (sum', "Found" : xs, s)
    go (sum', xs, s) (Sum a' b') =
      let a = (get sum' a')
          b = (get sum' b')
       in case S.lookup (a) s of
            S.E -> (sum', "0" : xs, S.E)
            t@S.Node {key = k} ->
              case S.lookup (b) t of
                S.E -> error "undefined"
                t1@S.Node {key = k1} ->
                  let totalSum = S.sum t
                      sum1 = if k >= (a) then S.sum (S.left t) else S.sum (S.left t) + k
                      sum2 = if k1 > (b) then S.sum (S.right t1) + k1 else S.sum (S.right t1)
                      r = totalSum - sum1 - sum2
                   in (r, show r : xs, t1)

-- >>> q4Runner
-- True
