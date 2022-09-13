 

import Data.Stream
 
public export
data  CoNat : Type  where
	Zero  :  CoNat
	Succ  :  (n : Inf CoNat) -> CoNat
 
public export
data  CoList : Type -> Type  where
	Nil  :  CoList a
	(::) : (x : a) -> (xs : Inf (CoList a)) -> CoList a

  
 
cycle'  : Nat -> CoList Nat
cycle' n = n :: (  cycle' n)



length : CoList a -> CoNat
length [] = Zero
length (x :: xs) =  Succ (length xs)


drop : Nat -> CoList a -> CoList a
drop Z xs = xs
drop _ [] = []
drop (S k) (x::xs) = drop k xs

filter : (a -> Bool) -> CoList a -> CoList a
filter p [] = []
filter p (x::xs) = if p x then x::(filter p xs) else filter p xs

filterFail : CoList Nat
filterFail = filter (<2) (cycle' 1)



zipStream : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStream f (x :: y) (z :: w) = (f x z) :: (zipStream f y w)

zipStreamList : (a -> b -> c) -> Stream a -> List b -> List c
zipStreamList f (x :: y)  [] = []
zipStreamList f (x :: y)  (z :: w) = (f x z) :: (zipStreamList f y w)

enumerate : List a -> List (Pair Nat a)
enumerate xs = zipStreamList(MkPair) natStream xs where
	natStream  :  Stream Nat
	natStream  =  nats_from 0
		where
			nats_from  :  Nat -> Stream Nat
			nats_from n = n :: nats_from (S n)



minus : CoNat -> CoNat -> CoNat
minus Zero b = Zero
minus x Zero = x
minus (Succ n) (Succ x) = minus n x -- not guarded by constructor

times : CoNat -> CoNat -> CoNat
times Zero b = Zero
times (Succ n) b = plus b (times n b) where 
	plus : CoNat -> CoNat -> CoNat
	plus Zero n  =  n
	plus (Succ m) n  =  Succ $ plus m n


fromNat : Nat -> CoNat
fromNat Z = Zero
fromNat (S k) = Succ (fromNat k)

toNat : CoNat -> Nat
toNat Zero = Z
toNat (Succ l) = S(toNat l) 

timesTest : Nat
timesTest = toNat $ times (fromNat 11) (fromNat 12)