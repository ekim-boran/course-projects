module Set.AVL where

import Set.BST

type AVL k v = BST Int k v

height E = 0
height (Node d _ _ _ _) = d

bf E = 0
bf (Node _ _ _ l r) = height l - height r

instance State Int where
  bin k v l r = Node (max (height l) (height r) + 1) k v l r
  balance E = E
  balance t@(Node h k v l r)
    | bf t == 2 && bf l == 1 = rotateRight t -- left left
    | bf t == -2 && bf r == -1 = rotateLeft t -- right right
    | bf t == 2 = rotateRight (bin k v (rotateLeft l) r) --left right
    | bf t == -2 = rotateLeft (bin k v l (rotateRight r))
    | otherwise = t
