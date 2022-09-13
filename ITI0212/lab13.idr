import Lecture11
import Lecture13

-- idris2 --package contrib lab13.idr


contrapositive : (a -> b) -> Not b -> Not a

contrapositive f g = g. f

dni : a -> Not $ Not a
dni a f = f a

dni' : Not (Not a) -> a 
dni' f = ?asd


tnr : Not $ Not $ Not a -> Not a
tnr n = n id

%hide Prelude.Stream.(::)
heads_differ : {x, y : a} -> { xs, ys : List a } -> Not (x = y) -> Not ((x :: xs) = (y :: ys))
heads_differ f = contrapositive path f where 
    path : {x, y : a} -> { xs, ys : List a } -> ((x :: xs) = (y :: ys)) -> (x = y) 
    path {xs = k } {ys = k} Refl = Refl 

-- find a way to erase implicit variables
tails_differ : {x, y : a} -> { xs, ys : List a } -> Not (xs = ys) -> Not (x :: xs = y :: ys)
tails_differ f = contrapositive path f where 
    path : {x, y : a} -> { xs, ys : List a } -> ((x :: xs) = (y :: ys)) -> (xs = ys) 
    path {xs = k } {ys = k} Refl = Refl 

 

odd_or_even : (n : Nat) -> Odd n `Or` Even n
odd_or_even Z = Right Z_even
odd_or_even (S k) = case odd_or_even k of
    Left x => Right $ succ_odd_even x
    Right x => Left $ succ_even_odd x