module Set.Splay where

import Prelude hiding (lookup, sum, tail, (!!))

data Splay k v
  = E
  | Node {_size :: !Int, _sum :: !k, key :: k, value :: v, left :: (Splay k v), right :: (Splay k v)}
  deriving (Ord, Eq, Show)

type Key t = (Ord t, Num t)

empty = E

size E = 0
size t = _size t

sum E = 0
sum t = _sum t

bin k v l r = Node (size l + size r + 1) (sum l + sum r + k) k v l r

zig (Node _ _ k1 v1 l1 r1) (Node {..}) = bin k1 v1 l1 (bin key value r1 right)
zig _ _ = error ""

zag (Node {..}) (Node _ _ k1 v1 l1 r1) = bin k1 v1 (bin key value left l1) r1
zag _ _ = error ""

lookup :: Key t => t -> Splay t v -> Splay t v
lookup _ E = E
lookup x t | x == key t = t
lookup x t@(Node {..}) | x < key = case lookup x left of
  E -> t
  l -> zig l t
lookup x t@(Node {..}) | otherwise = case lookup x right of
  E -> t
  r -> zag t r

find x t = case lookup x t of
  t@(Node {..}) | key == x -> (Just (key, value), t)
  t -> (Nothing, t)

insert :: (Key k) => k -> v -> Splay k v -> Splay k v
insert k v t =
  case lookup k t of
    E -> (bin k v E E)
    t | key t == k -> t
    (Node {..}) | k > key -> bin k v (bin key value left E) right
    (Node {..}) -> bin k v left (bin key value E right)

(!!) :: (Key k) => Splay k v -> Int -> (k, v)
(!!) E _ = error "index out of bounds"
(!!) t@(Node {..}) n | n > size t = error "index out of bounds"
(!!) (Node {..}) n | n == size left = (key, value)
(!!) (Node {..}) n | n < size left = left !! n
(!!) (Node {..}) n = right !! (n - (size left) - 1)

delete :: (Key k) => k -> Splay k v -> Splay k v
delete _ E = E
delete k t =
  case lookup k t of
    E -> error "splay tree corruption"
    t@(Node {..}) | k == key -> merge (left) (right)
    t -> t
  where

merge E r = r
merge l E = l
merge l r = case splayRight l of
  (Node {right = E, ..}) -> bin key value left r
  _ -> error "splay tree corruption"

splayRight E = E
splayRight h@(Node {right = E}) = h
splayRight (Node _ _ k1 v1 l1 (Node _ _ k2 v2 l2 r2)) = splayRight (bin k2 v2 (bin k1 v1 l1 l2) r2)

splay :: (Key k) => Splay k v -> Int -> Splay k v
splay E _ = error "index out of bounds"
splay t@(Node {..}) n | n > size t = error "index out of bounds"
splay t@(Node {..}) n | n == size left = t
splay t@(Node {..}) n | n < size left = case splay left n of
  E -> error "index out of bounds"
  l -> zig l t
splay t@(Node {..}) n = case splay right (n - size left + 1) of
  E -> error "index out of bounds"
  l -> zag t right

fromList :: (Key k) => [(k, v)] -> Splay k v
fromList [] = E
fromList l = foldl (\acc (k, v) -> insert k v acc) E l

toList t = go t []
  where
    go E = id
    go (Node _ _ k v l r) = (go l) . ((k, v) :) . (go r)

x = (insert 20 3 $ insert 10 2 $ insert 30 4 E)

-- >>>  lookup 31 (splay x 1)
-- Node {_size = 3, _sum = 60, key = 30, value = 4, left = Node {_size = 2, _sum = 30, key = 20, value = 3, left = Node {_size = 1, _sum = 10, key = 10, value = 2, left = E, right = E}, right = E}, right = E}
