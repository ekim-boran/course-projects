{-- 
 -- ITI0212 Lecture week 11, 2021.04.05
 -- 
 -- Propositions as Types
 --
 --}

module Lecture11

import Data.Nat  -- if using Idris 2 (for LTE)

%default total  -- needed for proof validity


{-  Predicates as Boolean-Valued Functions  -}


-- evenness as a boolean-valued function:
public export
is_even  :  Nat -> Bool
is_even Z  =  True
is_even (S Z)  =  False
is_even (S (S n))  =  is_even n


four_is_even  :  Bool
four_is_even  =  is_even 4

six_is_even  :  Bool
six_is_even  =  is_even 6


-- From the perspective of propositions as Booleans,
-- there are only two propositions, True and False.

true_is_true  :  Bool
true_is_true  =  four_is_even == six_is_even

-- Fermat's Last Theorem and 1 + 1 == 2 have the same value.




-- A different perspective is that
-- each logical proposition is distinct,
-- and that a proof of a proposition is
-- a conceptual object in its own right.




{-  Predicates as Indexed Types  -}


-- evenness as an Nat-indexed type:
public export
data  Even : (n : Nat) -> Type  where
	-- primitive evidence that zero is even:
	Z_even  :  Even Z
	-- primitive evidence that double successor preserves evenness:
	SS_even :  Even n -> Even (S (S n))


{-
                                                              SS_even
                                          SS_even            (SS_even
                      SS_even            (SS_even            (SS_even
   Z_even              Z_even              Z_even)             Z_even))

    0  |--->  1  |--->  2  |--->  3  |--->  4  |--->  5  |--->  6  ...
         S         S         S         S         S         S
 -}

-- We can construct elements of type Even n only when n is even,
-- and moreover each Even (2 * m) contains exactly one element.


four_even  :  Even 4
four_even  =  SS_even (SS_even Z_even)

six_even  :  Even 6
six_even  =  SS_even four_even



-- We can think of a term of a type as a proof of a proposition,
-- and we can manipulate these proofs in our programs
-- just like any other expressions.




{-  Proving Properties by Constructing Terms  -}


{-  On dependent pattern matching:

 * A *term* of type Even n acts as
   a *proof* that n is even.

 * A *context variable* of type Even n acts as
   an *assumption* that n is even.

 * We can use pattern matching to examine the possible
   constructor forms that such an assumption can have.

 * Because the type Even n is indexed by the type Nat,
   case analyzing an assumption of type Even n
   gives us information about the indexing Nat n as well.
 -}


-- The constructor SS_even says that the double-successor
-- of an even number is even.
-- We can prove the converse as well:
public export
pp_even  :  Even (S (S n)) -> Even n
pp_even (SS_even n_even)  =  n_even




{-  Proof Validity and Totality  -}

-- Only a total term of a type corresponds to
-- a valid proof of the respective proposition:
partial
everything_even  :  (n : Nat) -> Even n
everything_even n  =  everything_even n





-- The sum of two even numbers is even

{- On proof strategy:

 * Addition is defined by recursion on its first argument
   so we try doing induction on the assumption
   corresponding to the first summand.

 * The assumption Even m is indexed by the the Nat m
   and we can bring it into scope to give ourselves
   control over its name.
 -}
public export
even_plus_even  :  Even m -> Even n -> Even (m + n)
even_plus_even {m = Z} Z_even n_even  =  n_even
even_plus_even {m = S (S m)} (SS_even m_even) n_even  =
	let
		IH  =  even_plus_even m_even n_even
	in
		SS_even IH

--   Even $ S (S m) + n
-->  Even $ S (S m + n)
-->  Even $ S (S (m + n))



{-  On proof engineering:

 * For a variety of reasons, Idris may not show you all of
   the information you want about intermediate proof states.

 * We will learn about several strategies to deal with this.
   Today we focus on two.
-}


-- even times even is even:

-- by the "lemma" strategy
-- (providing local definitions using let or where):
namespace  By_lemma
	even_times_even  :  Even m -> Even n -> Even (m * n)
	even_times_even {m = Z} Z_even {n = n} n_even  =  Z_even
	even_times_even {m = S (S m)} (SS_even m_even) {n = n} n_even  =
		even_plus_even n_even n_plus_m_times_n_even
		where
			m_times_n_even  :  Even $ m * n
			m_times_n_even  =  even_times_even m_even n_even
			--
			n_plus_m_times_n_even  :  Even $ n + (m * n)
			n_plus_m_times_n_even  =  even_plus_even n_even m_times_n_even

%hide even_times_even


-- by the "explication" strategy
-- (providing implicit arguments explicitly):
namespace  By_explication
	even_times_even  :  Even m -> Even n -> Even (m * n)
	even_times_even {m = Z} Z_even {n = n} n_even  =  Z_even
	even_times_even {m = S (S m)} (SS_even m_even) {n = n} n_even  =
		even_plus_even {m = n} {n = n + (m * n)} n_even $
		even_plus_even {m = n} {n = m * n} n_even $
		even_times_even {m = m} {n = n} m_even n_even

%hide even_times_even



-- Once we have found a proof, we may choose to
-- inline the local definitions
-- or remove the explicated implicit arguments
-- in order to streamline the proof:
public export
even_times_even  :  Even m -> Even n -> Even (m * n)
even_times_even Z_even n_even  =  Z_even
even_times_even (SS_even m_even) n_even  =
	even_plus_even n_even $
	even_plus_even n_even $
	even_times_even m_even n_even








{-  Binary Relations as Boolean-Valued Functions  -}


-- ( â‰¤ ) as a boolean-valued function:
is_lte  :  Nat -> Nat -> Bool
is_lte Z n  =  True
is_lte (S m) Z  =  False
is_lte (S m) (S n)  =  is_lte m n

three_is_lte_five  :  Bool
three_is_lte_five  =  3 `is_lte` 5

four_is_lte_six  :  Bool
four_is_lte_six  =  4 `is_lte` 6




{-  Binary Relations as Indexed Types  -}


-- ( â‰¤ ) as a NatÃ—Nat-indexed type:
namespace Lte
	-- this is in the standard library as LTE
	--(in Prelude for Idris 1 and in Data.Nat for Idris 2)
	data  LTE'  :  (m : Nat) -> (n : Nat) -> Type  where
		-- primitive evidence that zero comes first:
		LTEZero'  :  LTE' Z n
		-- primitive evidence that successor preserves order:
		LTESucc'  :  LTE' m n -> LTE' (S m) (S n)


{-
  \ n   0               1               2               3    ...
  m /-----------------------------------------------------------
    |
  0 |  â‰¤Z              â‰¤Z              â‰¤Z              â‰¤Z
    |
  1 |                â‰¤S â‰¤Z           â‰¤S â‰¤Z           â‰¤S â‰¤Z
    |
  2 |                              â‰¤S (â‰¤S â‰¤Z)      â‰¤S (â‰¤S â‰¤Z)
    |
  3 |                                            â‰¤S (â‰¤S (â‰¤S â‰¤Z))
  . |
  . |
  . |
 -}

-- So for any m , n : Nat, the type LTE m n contains:
--
-- * exactly one element if m â‰¤ n,
-- * no elements otherwise.



two_lte_three  :  LTE 2 3
two_lte_three  =  LTESucc (LTESucc LTEZero)

three_lte_four  :  LTE 3 4
three_lte_four  =  LTESucc two_lte_three


-- We can interpret a term of type LTE m n
-- as a proof that m â‰¤ n.




{-  Some LTE Properties  -}


-- The constructor LTESucc says that if m â‰¤ n then S m â‰¤ S n.
-- We can prove the converse as well:
public export
lte_pred  :  LTE (S m) (S n) -> LTE m n
lte_pred (LTESucc m_lte_n)  =  m_lte_n


-- LTE is a reflexive relation:
public export
lte_refl  :  {n : Nat} -> LTE n n
lte_refl {n = Z}  =  LTEZero
lte_refl {n = S n}  =  LTESucc lte_refl


-- LTE is a transitive relation:
-- (note that Idris lets you reorder the implicit arguments)
public export
lte_trans  :  LTE l m -> LTE m n -> LTE l n
lte_trans {l = Z} LTEZero {m = m} m_lte_n {n = n}  =  LTEZero
lte_trans {l = S l} (LTESucc l_lte_m) {m = S m} (LTESucc m_lte_n) {n = S n} =
	LTESucc (lte_trans l_lte_m m_lte_n)


