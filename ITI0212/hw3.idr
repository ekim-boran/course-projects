
import Data.Vect
import Data.List.Elem
implementation Semigroup (a -> a) where
    (<+>) f g = g . f
implementation Monoid (a -> a) where
    neutral = id

applicify : {t : Type -> Type} -> Applicative t => (op : a -> a -> a) -> t a -> t a -> t a
applicify f a b = map f a <*> b

infixl 7 +?
(+?) : Num a => Maybe a -> Maybe a -> Maybe a
(+?) = applicify (+)

infixl 7 +*
(+*) : Num a => {n : Nat} -> Vect n a -> Vect n a -> Vect n a
(+*) = applicify (+)


mapPair : (f : a -> a') -> (g : b -> b') -> Pair a b -> Pair a' b'
mapPair f g (a, b) = (f a, g b)

mapDPair : (f : a -> a') -> (g : {x : a} -> b x -> b' (f x)) -> DPair a b -> DPair a' b'
mapDPair f g (x ** y) = (f x ** g y)

ary_op : (arity : Nat) -> Type -> Type
ary_op Z ty = ty
ary_op (S k) ty = ty -> (ary_op k ty)

--data Elem : a -> List a -> Type where
--    Here : Elem z (z :: xs)
--    There : Elem z xs -> Elem z (x :: xs)

list_eq : {xs : List a} -> xs = xs ++ []
list_eq {xs = []} = Refl
list_eq {xs = (x::xs)} = cong (x::) list_eq


in_left : Elem z xs -> (ys : List a) -> Elem z (xs ++ ys)
in_left Here ys = Here
in_left (There a) ys = There $ in_left a ys
 
in_right : Elem z ys -> (xs : List a) -> Elem z (xs ++ ys)
in_right y [] = y
in_right y (x::xs) = There $ in_right y xs

