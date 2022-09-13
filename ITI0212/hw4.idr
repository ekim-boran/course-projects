import Lecture12
import Lecture13
-- idris2 --package contrib hw4.idr
 
coerce  :  {0 a , b : Type} -> (path : a = b) -> a -> b
coerce  Refl  =  id

data Even : (n : Nat) -> Type where 
    Z_Even : Even Z
    SS_Even : Even n -> Even (S (S n))

double_even : (n : Nat) -> Even (n + n)
double_even 0 = Z_Even
double_even (S k) = let 
                        a = cong (Even . S) $ plus_succ_right {m = k} {n = k}
                        b = SS_Even $ double_even {n = k}
                    in
                    coerce (sym a) b

-- q2 
times_zero_right : (n : Nat) -> n * 0 = 0
times_zero_right {n = 0} = Refl
times_zero_right {n = (S k)} = times_zero_right {n = k}

 

--even_times_any : (m , n : Nat) -> Even m -> Even (m * n)
--even_times_any m Z _ = let
--                h =  cong Even $ (sym $ times_zero_right {n = m}) 
--        in
--    coerce h (Z_Even)
--even_times_any m (S k) _ = let
--                h =  cong Even $ (sym $ times_succ_right' {n = m}) 
--        in
--    ?asdasd                        

even_plus_even : Even n -> Even m -> Even (n + m)
even_plus_even {n = Z} Z_Even y_even = y_even
even_plus_even {n = S(S n)} (SS_Even a) y_even = SS_Even (even_plus_even a y_even)

y :  {n, k : Nat } -> Even (n + n + (k * n)) = Even (n + (n + (k * n)))  
y {n = n } {k = k} = let 
    x = sym (plus_assoc {l = n} {m = n} {n = k * n })
    in 
    cong Even x

even_times_any : (m , n : Nat) -> Even m -> Even (m * n)
even_times_any Z n Z_Even = Z_Even
even_times_any (S ( S k)) n (SS_Even e) = 
        coerce y x
    where 
        IH : Even (k * n)
        IH = even_times_any k n e 
        x : Even (n + n + (k * n))
        x = even_plus_even (double_even {n = n}) IH

numToEven : (n : Nat) -> Maybe (Even n)
numToEven Z = Just $ Z_Even
numToEven (S(S k)) =   map SS_Even (numToEven k)
numToEven _ = Nothing

etaTest :  (m , n : Nat) -> Maybe (Even (m * n))
etaTest a b = map (even_times_any a b) (numToEven a)



dm1 : Not a `Or` Not b -> Not (a `And` b)
dm1 (Left a) = \(a', b') =>  a a'
dm1 (Right b)= \(a', b') =>  b b'

dm2 : Not a `And` Not b -> Not (a `Or` b)
dm2 (a, b )= \x => case x of 
        Left a' => a a' 
        Right b' => b b'


 

evens_are_doubles : Even n -> Some Nat $ \ m => m + m = n
evens_are_doubles Z_Even = (0 **  Refl)
evens_are_doubles (SS_Even k) = 
    let 
        (k ** y) = evens_are_doubles k 
        x = cong S $ plus_succ_right {m = k } {n = k }
        y' = cong (S . S) y
    in
    (S k ** trans x y')


half : (n : Nat) -> {auto even : Even n} -> Nat


