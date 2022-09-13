import Data.Vect
import Lecture12
import Syntax.PreorderReasoning
%default total  --  needed to ensure proof validity

times_zero_left : (n : Nat) -> 0 * n = 0
times_zero_left {n = n} = Refl  

times_zero_right : (n : Nat) -> n * 0 = 0
times_zero_right {n = 0} = Refl
times_zero_right {n = (S k)} = times_zero_right {n = k}

times_succ_left : {m , n : Nat} -> (S m) * n = (m * n) + n
times_succ_left = plus_sym

plus2 : {m , n, l : Nat} ->  m + (n + l) = (n + (m + l) )
plus2 =
       let k =  cong (+l) (plus_sym {m = m} {n = n}) 
           x  = trans  plus_assoc k 
           v = trans x (sym plus_assoc) in
           v


times_succ_right' : {m , n : Nat} -> m * (S n) = m + (m * n)
times_succ_right'  {m = Z} = Refl
times_succ_right'  {m = S k} = 
    let a = times_succ_right' { m = k } {n = n}  
        b = cong (n+) a in
        cong S (trans b plus2)


 

times_sym : {m , n : Nat} -> m * n = n * m
times_sym {m = 0}= sym (times_zero_right {n = n})
times_sym {m = (S k)}= 
    let x = cong   (n +) (times_sym { m = k } {n = n }   ) 
        b = sym $ times_succ_right' {m = n } {n = k}
        y = trans x b
    in
    y


times_two :  {n : Nat} -> 2 * n = n + n
times_two  = cong (n+) plus_zero_right


double_length_vect : {n : Nat} -> Vect (2 * n) a = Vect (n + n) a
double_length_vect = cong (\n => Vect n a)   times_two

plus_s : { n : Nat } -> n + 1 = S n
plus_s { n = Z } = Refl
plus_s { n = S k } = cong S (plus_s)

public export
coerce  :  {0 a , b : Type} -> (path : a = b) -> a -> b
coerce  Refl  =  id

reverse_vect : {n : Nat} -> Vect n a -> Vect n a
reverse_vect {n = Z } [] = []
reverse_vect { n = S k } (x::xs) = coerce (cong (\n => Vect n a) plus_s) (xs ++ [x])
