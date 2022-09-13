module Course5.W3 where

import Data.List (nub)
import Data.Map qualified as M
import SAT.MiniSat

data Color = Red | Blue | Green deriving (Show, Eq, Ord, Enum)

data Node a = Node Int a deriving (Show, Eq, Ord)

-- each node has a single color
singleColor range nodes = concatMap (\n -> a n : b n) nodes
  where
    a n = foldl1 (:||:) [(Var (Node n i)) | i <- range] -- Each vertex has a color.
    b n = [Not (((Var (Node n i)) :&&: (Var (Node n j)))) | i <- range, j <- range, i < j] -- has only one color.

-- nodes with edges cannot belong to same color
edgeColor range edges = [Not (((Var (Node s c)) :&&: (Var (Node e c)))) | (s, e) <- edges, c <- [Red .. Green]]

q1 edges = fmap go $ solve $ foldl1 (:&&:) $ (singleColor range nodes) ++ edgeColor range edges
  where
    nodes = nub $ concat [[s, e] | (s, e) <- edges]
    range = [Red .. Green]
    go = M.keys . M.filter (id)

-- hamiltonian cycle

single n = concat [a n : b n | n <- range] ++ concat [c i : d i | i <- range]
  where
    range = [1 .. n]
    a n = foldl1 (:||:) [(Var (Node n i)) | i <- range] -- Each vertex belongs to a path.
    b n = [Not (((Var (Node n i)) :&&: (Var (Node n j)))) | i <- range, j <- range, i < j] -- Each vertex appears just once in a path.
    c i = foldl1 (:||:) [(Var (Node n i)) | n <- range] -- Each position in a path is occupied by some vertex.
    d i = [Not (((Var (Node n i)) :&&: (Var (Node n' i)))) | n <- range, n' <- range, n < n'] -- No two vertices occupy the same position of a path.

edgeConstraints n edges = concat [notConnected n n' | n <- range, n' <- range, n < n', not $ isConnected n n']
  where
    range = [1 .. n]
    isConnected x y = any (== (x, y)) edges
    notConnected n n' =
      [Not (((Var (Node n i)) :&&: (Var (Node n' j)))) | (i, j) <- zip range (tail range)]
        ++ [Not (((Var (Node n' i)) :&&: (Var (Node n j)))) | (i, j) <- zip range (tail range)]

q2 edges = M.keys . M.filter id <$> (solve $ foldl1 (:&&:) $ single nodes ++ edgeConstraints nodes edges)
  where
    nodes = maximum $ concat [[s, e] | (s, e) <- edges]

q3 xs = solve $ foldl1 (:&&:) $ fmap go xs
  where
    go (xs, r) = foldl1 (:||:) $ fmap (foldl1 (:&&:)) $ fmap (fmap fst) $ filter ((<= r) . sum . fmap snd) $ go' [(i, x) | (i, x) <- zip [0 ..] xs, x /= 0]
    go' [] = [[]]
    go' ((i, x) : xs) = (((Not $ Var i, 0) :) <$> go' xs) ++ (((Var i, x) :) <$> go' xs)

edges = [(1, 2), (2, 3), (1, 3)]

edges2 = [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]

edges3 = zip xs (tail xs)
  where
    xs = [1 .. 10]

-- >>> q1 edges
-- Just [Node 1 Green,Node 2 Blue,Node 3 Red]
-- >>> q2 edges2
-- Just [Node 1 4,Node 2 3,Node 3 2,Node 4 1]
--
test1 = zip [[5, 2, 3], [-1, -1, -1]] [6, -2]

test2 = zip [[1, 0, 0], [0, 1, 0], [0, 0, 1]] [1, 1, 1]

-- >>> q3 test2
-- Just (fromList [(0,False),(1,False),(2,False)])
--
