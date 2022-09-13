
import Data.Vect 
import Data.Fin

countVec : (a -> Bool) -> Vect n a ->Nat 
countVec f [] = 0
countVec f (x::xs) = if f x then S(countVec f xs) else countVec f xs

filterVect : (p : a -> Bool) -> (xs : Vect n a) -> Vect (countVec p xs) a 
filterVect p [] = []
filterVect p (x :: xs) with (p x)
    filterVect p (x :: xs) | True = x :: filterVect p xs
    filterVect p (x :: xs) | False = filterVect p xs

filterVect2 : (p : a -> Bool) -> (xs : Vect n a) -> DPair Nat (\n => Vect n a)
filterVect2 p [] = (Z ** [])
filterVect2 p (x :: xs) = let 
   (n ** ys) = filterVect2 p xs in
   if p x then (S n ** x::ys) else (n ** ys)
   
--- labs

indPair : (a, b) -> DPair a (\x => b)
indPair (x, y) =  (x ** y)

DisjointUnion : Type -> Type -> Type
DisjointUnion a b = DPair Bool (\ i => if i then b else a)

fromDU : (a -> c) -> (b -> c) -> DisjointUnion a b -> c
fromDU f g (a ** b) = if a then g b else f b

fromEither : Either a b -> DisjointUnion a b
fromEither (Left x) = (False ** x)
fromEither (Right x) = (True ** x)

toEither : DisjointUnion a b -> Either a b
toEither (  fst  ** snd) = if fst then Right snd else Left snd

record  DUrec a b where
	constructor  MkDUrec
	bool : Bool
	ty : if bool then b else a


fromDUrec : DUrec a b -> DisjointUnion a b
fromDUrec (MkDUrec bool ty) = (bool ** if bool then ty else ty)

toDUrec : DisjointUnion a b -> DUrec a b
toDUrec (b ** x) =  MkDUrec b (if b then x else x)
 

arySum : (n : Nat) -> Vect n Type -> Type
arySum n xs = DPair (Fin n) (\n => index n xs)

xx : Fin 3
xx = 0
 
 
--b = the (3 `arySum` [Unit , Bool , Nat]) (1 ** True)


data HVect : Vect n Type -> Type where
    Nil : HVect []
    (::) : t -> HVect ts -> HVect (t :: ts)


head : HVect (x::xs) -> x 
head (x::xs) = x
 

tail : (HVect (x::xs)) -> HVect xs
tail (x::xs) = xs

(++) : HVect xs -> HVect ys -> (HVect (xs ++ ys))
(++) [] b = b
(++) (x :: y) b =  x:: (y ++ b)

myindex : (x : Fin n) -> HVect xs ->  (index x xs)
myindex FZ (x::xs) = x
myindex (FS a) (x::xs) = myindex a xs

