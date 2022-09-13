module Lecture9
import Data.Vect

 
infixr 10 \/
public export 
data Set : Type -> Type where
  Empty : Set a
  (\/) : a -> Set a -> Set a

implementation Show a => Show (Set a) where
    show x = "{ " ++ comma_sep x ++ " }" where
      comma_sep : Set a -> String
      comma_sep Empty = ""
      comma_sep (x \/ Empty) = show x
      comma_sep (x \/ (y \/ z)) = show x ++ ", " ++ comma_sep (y \/ z)

-- useful since we can steal list syntax

implementation Cast (List a) (Set a) where
        cast [] = Empty
        cast (x :: xs) = x \/ cast xs

-- check if an element is in a set
is_in : Eq a => a -> Set a -> Bool
is_in x Empty = False
is_in x (y \/ z) = if (x == y) then True else is_in x z

-- check if all the elements of one sets are in a second set
is_subset_of : Eq a => Set a -> Set a -> Bool
is_subset_of Empty ys = True
is_subset_of (x \/ y) ys = (x `is_in` ys) && (y `is_subset_of` ys)

-- set equality
implementation Eq a => Eq (Set a) where
   xs == ys = (xs `is_subset_of` ys) && (ys `is_subset_of` xs)

{-
interface Semigroup ty where
  (<+>) : ty -> ty -> ty


<+> is assumed to be *associative*
-}


-- set union
set_union : Set a -> Set a -> Set a
set_union Empty ys = ys
set_union (x \/ y) ys = x \/ (set_union y ys)
public export 
implementation Semigroup (Set a) where
  (<+>) = set_union


{-

interface Semigroup ty => Monoid ty where
  neutral : ty

neutral <+> x = x = x <+> neutral

-}
public export 
implementation Monoid (Set a) where
  neutral = Empty

{-
interface Functor (f : Type -> Type) where
c
Functor List where
    map f []      = []
    map f (x::xs) = f x :: map f xs
-}

implementation [MyListMap] Functor List where
  map f []      = []
  map f (x::xs) = f x :: map f xs

-- trees, a different approach

{-
   c
 /  \
a    b
-}

data Tree: Type -> Type where
  Leaf: (label: a) -> Tree a
  Node: (label: a) -> (child1: Tree a) -> (child2: Tree a) -> Tree a

implementation Show a => Show (Tree a) where
  show (Leaf label) = "[" ++ show label ++ "]"
  show (Node label child1 child2) = "[" ++ show child1 ++ show label ++ show child2 ++ "]"

implementation Functor Tree where
  map f (Leaf label) = Leaf (f label)
  map f (Node label child1 child2) = Node (f label) (map f child1) (map f child2)
public export 
implementation Functor Set where
  map f Empty = Empty
  map f (x \/ y) = (f x) \/ (map f y)

{-
interface Functor f => Applicative (f : Type -> Type) where
    pure  : a -> f a
    (<*>) : f (a -> b) -> f a -> f b

(<*>) is a kind of generalised function application

-}

implementation [MyMaybeAppl] Applicative Maybe where
  pure x = Just x
  (Just f) <*> (Just x) = Just (f x)
  _ <*> _ = Nothing

-- <*> : List (a->b) -> List a -> List b
-- [f1, f2, f3] <*> [a1, a2] = [f1 a1, f2 a2]
-- [f1, f2, f3] <*> [a1, a2] = [f1 a2, f1 a2, f2 a1, f2 a2, f3 a1, f3 a2]



makeConstantVect : (k: Nat) -> a -> Vect k a
makeConstantVect Z x = []
makeConstantVect (S k) x = x :: (makeConstantVect k x)

-- replicate


implementation [MyVectAppl] Applicative (Vect k) where
  pure x = ?pure_vect_rhs
  vf <*> vx = ?appl_vect_rhs

  -- zipWith

public export 
implementation Applicative Set where
    pure a = a \/ Empty
    (f \/ fs) <*> xs = (map f xs) `set_union` (fs <*> xs)
    Empty <*> _ = Empty

{-

interface Applicative m => Monad (m : Type -> Type) where

    (>>=)  : m a -> (a -> m b) -> m b

(>>=) usually called bind

-}

implementation [MyMaybeMonad] Monad Maybe where
  Nothing >>= g = Nothing
  (Just x) >>= g = g x

maybe_add : (Num a) => Maybe a -> Maybe a -> Maybe a
maybe_add m n = m >>= (\x => (n >>= (\y => pure (x+y))))

{-
  Inside a do block, the following syntactic transformations are applied:

  x <- v; e becomes v >>= (\x => e)
  v; e becomes v >>= (\_ => e)
  let x = v; e becomes let x = v in e
-}

maybe_add_do : (Num a) => Maybe a -> Maybe a -> Maybe a
maybe_add_do m n = do
                    x <- m
                    y <- n
                    pure (x+y)
public export 
implementation Monad Set where
   Empty >>= f = Empty
   (x \/ ys) >>= f = (f x) <+> (ys >>= f)

intersection : Eq a => Set a -> Set a -> Set a
intersection sx sy = do
                      x <- sx
                      y <- sy
                      if (x==y) then pure x else Empty


-- show $ (cast [0,1,2,3]) `intersection` (cast [2,3,4,5])


cartesianProduct : Set a -> Set b -> Set (Pair a b)
cartesianProduct sx sy = do
                           x <- sx
                           y <- sy
                           pure (x,y)

-- show $ (cast [0,1,2,3]) `cartesianProduct` (cast [2,3,4,5])

disjointUnion : Set a -> Set b -> Set (Either a b)
disjointUnion sx sy = ?disjoint_union_rhs

-- show $ (cast [0,1,2,3]) `disjointUnion` (cast [2,3,4,5])


canonical_seq : Monad m => m(a -> b) -> m a -> m b
canonical_seq mf mx = do
                       f <- mf
                       x <- mx
                       pure (f x)
