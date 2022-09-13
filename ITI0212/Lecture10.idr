{-- 
 -- ITI0212 Lecture week 10, 2021.03.29
 -- 
 -- Programming with Dependent Types
 --
 --}

module Lecture10

import Data.Nat
import Data.Fin
import Data.Vect


-- Recall that we can filter a List
-- using a predicate:
filterList  :  (a -> Bool) -> List a -> List a
filterList p []  =  []
filterList p (x :: xs)  =
	let
		rest  =  filterList p xs
	in
		case  p x  of
			True  =>  x :: rest
			False  =>  rest


namespace Stuck1
	-- If we try to filter a Vect the same way then we get stuck,
	-- not in the program, but in the type:
	filterVect  :  (a -> Bool) -> Vect n a -> Vect ?len a
	filterVect p []  =  ?stuck_1_1 -- []
	filterVect p (x :: xs)  =
		let
			rest  =  filterVect p xs
		in
			case  p x  of
				True  =>  ?stuck_1_2 -- x :: rest
				False  =>  ?stuck_1_3 -- rest




{-- Dependent Pair Types --}

namespace Dp
	-- Recall that a Pair type packages up
	-- two *independent* pieces of data in its factors:
	data  Pair' : (a : Type) -> (b : Type) -> Type  where
		MkPair'  :  (x : a) -> (y : b) -> Pair' a b


	-- A dependent pair type (DPair) is like a Pair,
	-- except that the *type* of the second factor
	-- can depend on the *value* of the first factor:
	data  DPair' : (a : Type) -> (b : a -> Type) -> Type  where
		MkDPair'  :  (x : a) -> (y : b x) -> DPair' a b


two_in_three  :  DPair Nat (\ n => Fin n)
two_in_three  =  (3 ** 2)


-- We can use a dependent pair to specify the return type
-- of the filter function for Vects:
filterVectDP  :  (a -> Bool) -> Vect n a -> DPair Nat (\ len => Vect len a)
filterVectDP p []  =  (0 ** [])
filterVectDP p (x :: xs)  =
	let
		rest  =  filterVectDP p xs
	in
		case  p x  of
			True  =>  (S (fst rest) ** x :: snd rest)
			False  =>  rest


-- DPair types are very useful but
-- can get cumbersome when iterated.




{-- Record Types --}

-- Record types are like iterated DPair types
-- with some extra convenience features, including:
-- * you can name their constructors,
-- * they generate individual namespaces,
-- * they include a projection function for each field (getters),
-- * they have syntactic sugar for updating fields (setters).


-- a record with two String fields:
record  Name  where
	constructor  MkName
	first : String
	last : String


-- Records can be nested:
record  Person  where
	constructor MkPerson
	name : Name
	email : String


-- constructor syntax:
ed  :  Person
ed  =  MkPerson (MkName "Ed" "Morehouse") "edmore@ttu.ee"


-- projection syntax:
old_email  :  String
old_email  =  ed.email


-- update syntax:
new_ed  :  Person
new_ed  =  record {email = "edward.morehouse@taltech.ee"} ed


-- Record types can be parameterized by other types.


-- A record type for finite sequences:
record  FiniteSequence a  where
	constructor  Seq
	length  :  Nat
	sequence  :  Vect length a


-- We can use this record type to specify the return type
-- of the filter function for Vects:
filterVectRec  :  (a -> Bool) -> Vect n a -> FiniteSequence a
filterVectRec p []  =  Seq 0 []
filterVectRec p (x :: xs)  =
	let
		rest  =  filterVectRec p xs
	in
		case  p x  of
			True  =>  record {length $= S , sequence $= (x :: )} rest
			False  =>  rest




-- Using a dependent pair (or equivalently, a record)
-- to specify the return type of filtering a Vect is okay,
-- but still pretty imprecise because there's nothing
-- connecting the size of the returned Vect to filtering.
-- For example, we could always just return (0 ** []).
--
-- Compare this with Vect mapping where the type guarantees
-- that the result will have the same size as the argument.
--
-- In order to be more precise about the return type
-- we will need to be able to compute it based on
-- the intended behavior of the filter function.




{-- Dependent Function Types --}

-- A dependent function is one whose return type
-- is computed from the value of its argument.


-- a function whose return type depends on its argument:
divide : (b : Bool) -> Integer -> Integer -> 
	if b then (Pair Integer Integer) else Double
divide True m n  =  (m `div` n , m `mod` n)
divide False m n  =  cast m / cast n


-- Recall that converting a Vect to a List is easy:
forget_length : Vect n a -> List a
forget_length []  =  []
forget_length (x :: xs)  =  x :: forget_length xs


-- But to convert a List to a Vect we need to compute the length.
-- We can do this using a dependent function:
learn_length  :  (xs : List a) -> Vect (length xs) a
learn_length [] = []
learn_length (x :: xs) = x :: learn_length xs



-- a function to count the number of elements
-- of a Vect for which a predicate is True:
%hide count  --  Prelude.count will confuse things
count  :  (a -> Bool) -> Vect n a -> Nat
count p []  =  0
count p (x :: xs)  =
	case p x of
		True  =>  S (count p xs)
		False  =>  count p xs


-- We can use this to specify the return type
-- of the filter function for Vects.

namespace Stuck2
	-- (first attempt)
	filterVectComp : (p : a -> Bool) -> (xs : Vect n a) -> Vect (count p xs) a
	filterVectComp p []  =  []
	filterVectComp p (x :: xs)  =
		case p x of
			True => ?stuck_2_1  --  goal type is
			False => ?stuck_2_2 --  not being refined


-- The problem is that Idris is not computing in the types
-- to account for which case we're in.

-- Such computation in types is triggered by pattern matching
-- on the *left* of the ` = ` for a clause.

-- The problem is that `p x` is is not a pattern
-- because `p` is not a constructor.

-- Idris lets us pattern match on arbitrary expressions
-- of inductive types using the `with` construct.


-- (second attempt, using `with`)
filterVectComp : (p : a -> Bool) -> (xs : Vect n a) -> Vect (count p xs) a
filterVectComp p []  =  []
filterVectComp p (x :: xs) with (p x) -- triggers computation in the type
	filterVectComp p (x :: xs) | True  =  x :: filterVectComp p xs
	filterVectComp p (x :: xs) | False  =  filterVectComp p xs

