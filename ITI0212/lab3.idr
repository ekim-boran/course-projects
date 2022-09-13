import Data.Vect
import Data.Nat
swapPair : (a, b) -> (b, a)
swapPair (a, b) = (b, a)

swapEither : (Either a b) -> (Either b a)
swapEither (Left x) = Right x
swapEither (Right x) = Left x

there : Nat -> List Unit
there Z =  Nil
there (S k) =  ()::there(k)

back : List Unit -> Nat
back [] =   Z
back (x :: xs) =   S(back xs)

test1: Nat
test1 = back $ there 12

project : Fin n -> Nat
project FZ = 0
project (FS x) = S (project x)

listify : Vect n a -> List a
listify [] =   []
listify (x :: xs) =  x :: (listify xs)

reverseList : List a -> List a
reverseList = go [] where 
    go : List a -> List a -> List a
    go acc [] = acc
    go acc (x::xs) = go (x::acc) xs

-- the definition is from standart library 
-- look again later
reverseVect : Vect len x -> Vect len x
reverseVect xs = go [] xs
  where go : Vect n x -> Vect m x -> Vect (n+m) x
        go {n}         acc []        = rewrite plusZeroRightNeutral n in acc
        go {n} {m=S m} acc (x :: xs) = rewrite sym $ plusSuccRightSucc n m
                                       in go (x::acc) xs


data Tree : Type -> Type where
  Leaf : Tree a
  Node : Tree a -> a -> Tree a -> Tree a

size : Tree a -> Nat
size Leaf =   0
size (Node l x r) =  size l + 1 + size r
depth : Tree a -> Nat
depth Leaf =   0
depth (Node l  x r ) = if l' < r' then r' + 1 else l' +1  where 
    l' : Nat
    l' = depth l 
    r' : Nat
    r' = depth r

flatten : Tree a -> List a
flatten = go [] where 
    go : List a -> Tree a -> List a
    go acc Leaf = acc
    go acc (Node l x r) = go (x::(go acc r)) l

flattenTest : List Int
flattenTest =  flatten $ Node (Node Leaf 1 Leaf) 3 (Node (Node Leaf 5 Leaf) 6 (Node Leaf 12 Leaf))