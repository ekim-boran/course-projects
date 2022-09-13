{-- 
 -- ITI0212 Lecture week 13, 2021.04.19
 -- 
 -- First-Order Logic
 --
 --}


module Lecture13

import Data.Nat
import Data.Fin
import Lecture11
import Lecture12

%default total  --  needed to ensure proof validity



{-  Falsity  -}

-- In the propositions-as-booleans interpretation
-- we can assert not only that something is true,
-- but also that it is false:

two_is_three  :  Bool
two_is_three  =  2 == 3


-- How can we assert that something is false
-- in the propositions-as-types interpretation?

Two_Is_Three  :  Type
Two_Is_Three  =  2 = 3


{-
 * Recall that we represent a proposition by a type and 
   a proof of a proposition by an element of that type.

 * One way to assert that a proposition is false
   is to claim that there can be no proofs of it.

 * A proposition with no proofs corresponds to
   a type with no elements; i.e. an empty type.
 -}


-- Recall the empty type  Void  from lecture 2:
data  Void' : Type  where
	--  no constructors!


-- Writing a function from the  Void  type to another type
-- is trivial because there are no cases to consider.

-- (this is in the standard library as  void)
trivial  :  Void -> a
trivial x  impossible


-- But writing functions to the  Void  type  from another type
-- is possible only if:


-- (1) the function is not total:
partial
hopeless  :  Nat -> Void   --  this function has the right type,
hopeless n  =  hopeless n  --  but it will never return a value


-- (2) the other type is also empty:

pointless  :  Fin 0 -> Void
pointless i  impossible




{-  Negation as Refutation  -}

-- We can use a total function to the  Void  type to express
-- the negation of a proposition interpreted as a type.
-- (this is in the Prelude as  Not)
Not'  :  Type -> Type
Not' a  =  a -> Void

{-
 * An element of type  Not a  is a function sending
   elements of type  a  to elements of type  Void.

 * But there are no elements of type  Void.

 * So if this function is total then there can be
   no elements of type  a  either.

 * Thus  a  must be an empty type.

 * If we interpret the type  a  as a proposition,
   this means that it can have no proofs.

 * Which is what it means to assert that it is false.
 -}


Two_Not_Three :  Not (2 = 3)
Two_Not_Three Refl   impossible


-- The logical "principle of explosion"
-- or "ex falso quodlibet" asserts that
-- from the combination of a proposition
-- and its negation we may conclude anything.

public export
contradiction  :  a -> Not a -> b
contradiction a_true a_false  =  void $ a_false a_true



-- In the propsitions-as-booleans interpretation
-- we can use the boolean negation function to define
-- the predicate of a number bing odd:
public export
is_odd  :  Nat -> Bool
is_odd  =  not . is_even


-- In the propsitions-as-types interpretation
-- we can do the same thing using type-level negation:
public export
Odd  :  Nat -> Type
Odd  =  Not . Even


-- Of course, one is odd:
public export
one_odd  :  Odd 1
one_odd Z_even  impossible
one_odd (SS_even n)  impossible


-- We can show that the successor of an even number is odd
-- by induction on the assumption  Even n:
public export
succ_even_odd  :  Even n -> Odd (S n)
succ_even_odd Z_even  =  one_odd
succ_even_odd (SS_even n_even)  =
	let
		IH = succ_even_odd n_even
	in
		IH . pp_even



-- We can show that the successor of an odd number is even,
-- but we can't pattern match on the assumption  Odd n
-- because  Odd  is not an inductively defined type.
-- However, we can pattern match on its  Nat  index  n:
public export
succ_odd_even  :  {n : Nat} -> Odd n -> Even (S n)
succ_odd_even {n = Z} zero_odd  =  contradiction Z_even zero_odd
succ_odd_even {n = S Z} one_odd  =  SS_even Z_even
succ_odd_even {n = S (S n)} ssn_odd  =
	let
		IH = succ_odd_even {n = n} $ ssn_odd . SS_even
	in
		SS_even IH



{-
 - Idris provides an interface for empty types:

	Uninhabited  :  Type -> Constraint
		uninhabited  :  {a : Type} -> Uninhabited a => a -> Void
 -}


-- lemma: the successor function is injective:
public export
pred_equal  :  S m = S n -> m = n
pred_equal Refl  =  Refl

-- The standard library includes implementations of  Uninhabited
-- for many types corresponding to false propositions,
-- including  Z = S n  and  S n = Z.
-- We can extend this to any differing Nats:



--public export
--implementation  Uninhabited (m = n) => Uninhabited (S m = S n)  where
--	uninhabited  =  uninhabited . pred_equal


-- The Uninhabited interface provides a method,
-- absurd : Uninhabited a => a -> b.
-- This is like the function  void  for the type  Void,
-- but for arbitrary empty types.

obvious  :  Not (42 = S Z)
obvious = absurd




{-- Logical Connectives  --}

-- A proof of the proposition  a And b
-- is a proof of the proposition  a
-- together with a proof of the proposition  b.
public export
And  :  (a : Type) -> (b : Type) -> Type
And  =  Pair

eg_and  :  (Even 2) `And` Not (7 = S 7)
eg_and  =  (SS_even Z_even , absurd)


-- A proof of the proposition  a Or b
-- is either a proof of the proposition  a
-- or else a proof of the proposition  b.
public export
Or  :  (a : Type) -> (b : Type) -> Type
Or  =  Either

eg_or  :  (Even 2) `Or` (7 = S 7)
eg_or  =  Left $ SS_even Z_even


-- A proof of the proposition  a Implies b
-- is a function sending any proof of the proposition  a
-- to a proof of the proposition  b.
public export
Implies  :  (a : Type) -> (b : Type) -> Type
Implies a b  =  a -> b

eg_implies  :  (7 = S 7) `Implies` (8 = S 8)
eg_implies path  =  cong S path



{-- Logical Quantifiers  --}


-- A proof of the proposition that some element
-- of the type  a  satisfies the predicate  p
-- is an element  x  of type  a  together with
-- a proof of the proposition that  x  satisfies  p.
public export
Some  :  (a : Type) -> (p : a -> Type) -> Type
Some  =  DPair

eg_some  :  Some Nat Even
eg_some  =  (S (S Z) ** SS_even Z_even)


-- A proof of the proposition that each element
-- of the type  a  satisfies the predicate  p
-- is a function that given any element  x  of type  a
-- produces a proof of the proposition that  x  satisfies  p.
public export
Each  :  (a : Type) -> (p : a -> Type) -> Type
Each a p  =  (x : a) -> p x

-- recall from lab 11:
succ_larger  :  {n : Nat} -> LTE n (S n)
succ_larger {n = Z}  =  LTEZero
succ_larger {n = S n}  =  LTESucc succ_larger

eg_each  :  Each Nat $ \ n => Some Nat $ \ m => LTE n m `And` Not (n = m)
eg_each n =  (S n ** (succ_larger , nope))
	where
		nope : Not (n = S n)
		nope path  impossible


-- The kind of constructive logic that this
-- propositions-as-types interpretation realizes
-- is called "intuitionistic logic".


