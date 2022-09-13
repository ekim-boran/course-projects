module Lecture12

import Data.Vect
import Syntax.PreorderReasoning

%default total  --  needed to ensure proof validity




namespace Equals
	data  Equal' : (x : a) -> (y : b) -> Type  where
		--  primitive evidence that anything is equal to itself:
		Refl'  :  Equal' x x

four_is_four  :  2 + 2 = 2 * 2
four_is_four  =  Refl

public export
plus_zero_left  :  {n : Nat} -> 0 + n = n
plus_zero_left  =  Refl

public export
plus_succ_left  :  {m , n : Nat} -> (S m) + n = S (m + n)
plus_succ_left  =  Refl


namespace  Scratch_1
	-- adding a zero on the right (naive version):
	private
	plus_zero_right  :  {n : Nat} -> n + 0 = n
	plus_zero_right {n = Z}  =  Refl
	plus_zero_right {n = S n}  =   
		 let
		 	IH  =  plus_zero_right {n = n}
		 in
		 	succ_equal IH
		 where
		 	succ_equal  :  i = j -> S i = S j
		 	succ_equal {i = k} Refl {j = k}  =  Refl

%hide plus_zero_right  --  so we can reuse the name

public export
congruence  :  (0 f : a -> b) -> (path : x = y) -> f x = f y
congruence f Refl  =  Refl

%hint
public export
plus_zero_right  :  {n : Nat} -> n + 0 = n
plus_zero_right {n = Z}  =  Refl
plus_zero_right {n = S n}  =  cong S plus_zero_right


-- adding a successor on the right:
%hint
public export
plus_succ_right  :  {m , n : Nat} -> m + (S n) = S (m + n)
plus_succ_right {m = Z} {n = n}  =  Refl
plus_succ_right {m = S m} {n = n}  =  congruence S (plus_succ_right {m =m } {n = n})

 
symmetry  :  x = y -> y = x
symmetry Refl  =  Refl

-- Idris has a version of  symmetry  in the Prelude called  sym.


-- Equality is a transitive relation:
transitivity  :  from = via -> via = to -> from = to
transitivity Refl Refl =  Refl

-- Idris has a version of  transitivity  in the Prelude called  trans.



-- We can use  transitivity  to write equality proof
-- terms in a more human-friendly way,
-- by relying on algebra rather than computation.
public export
plus_sym   :  {m , n : Nat} -> m + n = n + m
plus_sym  {m = 0} {n = n} = sym plus_zero_right
plus_sym  {m = (S k)} {n = n} = 
	let 
		x = cong S (plus_sym  {m = k } {n = n})
		y = sym (plus_succ_right  {m = n } {n = k})	in transitivity x y

---- associativity of addition (by induction on m):
%hint
public export
plus_assoc  :  {l , m , n : Nat} -> l + (m + n) = (l + m) + n
plus_assoc {m = Z}  =  Calc $
	|~ l + (Z + n)
	~~ l + n             ...(cong (l + ) plus_zero_left)
	~~ (l + Z) + n       ...(cong ( + n) $ sym plus_zero_right)
plus_assoc {m = S m}  =  Calc $
	|~ l + (S m + n)
	~~ l + S (m + n)     ...(cong (l + ) plus_succ_left)
	~~ S (l + (m + n))   ...(plus_succ_right)
	~~ S ((l + m) + n)   ...(cong S plus_assoc)
	~~ S(l + m) + n      ...(sym plus_succ_left)
	~~ (l + S m) + n     ...(cong ( + n) $ sym plus_succ_right)

{--  Equality of Types  --}


-- An indexed type is a function from the indexing type to Type.
-- So we can use conguence to turn an equality between indices
-- into an equality between types.

--vect_length_sym  :  {m , n : Nat} -> Vect (m + n) a = Vect (n + m) a
--vect_length_sym  =  cong (\ len => Vect len a) plus_sym
--
--
--
---- If we know that two types are equal
---- then we can write a coercion function between them:
--public export
--coerce  :  {0 a , b : Type} -> (path : a = b) -> a -> b
--coerce  Refl  =  id
--
--
--
--namespace  Scratch_3
--	private
--	-- twisted Vect concatenation (using coerce):
--	twisted_concat  :  {m , n : Nat} -> Vect m a -> Vect n a -> Vect (n + m) a
--	twisted_concat xs ys  =  coerce vect_length_sym (xs ++ ys)
--
--%hide twisted_concat  --  so we can reuse the name
--
--
---- This is a common pattern.
---- Idea:
---- (1) Congruence says:
----     if the indices are equal then the indexed types are equal.
---- (2) Coercion says:
----     if the indexed types are equal then there is a function between them.
----
---- Combining these is called "transport":
--public export
--transport  :  {0 a : Type} -> (0 fiber : a -> Type) ->
--	{0 x , y : a} -> (path : x = y) ->
--	fiber x -> fiber y
--transport fiber Refl  =  id
--
--{-
--            fiber x                                 fiber y            
--           _________                               _________           
--          /         \                             /         \          
--         /     p     \    transport fiber path   / transport \         
--        |             |   ------------------->  |  fiber path |        
--         \           /                           \     p     /         
--          \_________/                             \_________/          
--                                   path                                
--               x    ===============================    y         : a   
--                                                                       
-- -}
--
--
--
---- Now we can write twisted vector concatenation as a one-liner:
--
---- twisted Vect concatenation (using transport):
--twisted_concat  :  {m , n : Nat} -> Vect m a -> Vect n a -> Vect (n + m) a
--twisted_concat xs ys  =  transport (\ len => Vect len a) plus_sym (xs ++ ys)
--
--{-
--        Vect (m + n) a                          Vect (n + m) a         
--           _________                               _________           
--          /         \     transport (Vect _ a)    /         \          
--         /           \         plus_sym          /           \         
--        |  xs ++ ys   |   ------------------->  |             |        
--         \           /                           \           /         
--          \_________/                             \_________/          
--                               plus_sym                                
--             m + n  ===============================  n + m       : Nat 
--                                                                       
-- -}
--
--
---- Idris has a version of  transport  in the standard library
---- called  Builtin.replace.
---- Inconveniently, it takes the type constructor
---- as an implicit argument called  p.
--
--