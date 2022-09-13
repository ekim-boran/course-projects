import Data.List
import Lecture9
implementation Semigroup Bool where 
    (<+>) False _ = False
    (<+>) True x = x

implementation Monoid Bool where 
    neutral = True


reduce : (Monoid a) => List a -> a
reduce = foldr (<+>) neutral

r1:Bool
r1 = reduce [True , False , True]
r2: String
r2 = reduce ["hello " , "brave " , "new " , "world"]
r3 : String
r3 = reduce $ the (List String) []
 
disjointUnion : Set a -> Set b -> Set (Either a b)
disjointUnion s1 s2 = (map Left s1) <+> map Right s2

join : Set (Set a) -> Set a
join = (>>= id)
join' : Monad t => t (t a) -> t a
join' = (>>= id)


data Tree: Type -> Type where
    Leaf: (label: a) -> Tree a
    Node: (label: a) -> (child1: Tree a) -> (child2: Tree a) -> Tree a


glue : Tree a -> Tree a -> Tree a -> Tree a
glue (Leaf x) l r = Node x l r
glue (Node x y z) l r = Node x (glue y l r) (glue z l r)

implementation Functor Tree where 
    map f (Leaf a) = Leaf (f a)
    map f (Node a l r) = Node (f a) (map f l) (map f r)

implementation Applicative Tree where 
    pure a = Leaf a
    (<*>) (Leaf f) x = map  f  x
    (<*>) (Node f l r) x = glue (map f x) (l <*> x) (r <*> x)

implementation Monad Tree where 
    (>>=) (Leaf x) f = f x
    (>>=) (Node x l r) f = glue (f x) (l >>= f) (r >>= f)

sapling: Unit -> Tree Unit
sapling () = Node () (Leaf ()) (Leaf ())

depth: Tree a -> Nat
depth (Leaf label) = 1
depth (Node label child1 child2) = 1 + max (depth child1) (depth child2)

generate : Nat -> Tree () 
generate Z = Leaf ()
generate (S k) = generate k >>= (\_ => sapling ())

testd : Nat
testd = depth $ generate 4

 