data Even : (n : Nat) -> Type where 
    Z_Even : Even Z
    SS_Even : Even n -> Even (S (S n))

four_is_even : Even 4 
four_is_even = SS_Even (SS_Even Z_Even)

pp_even : Even (S (S n)) -> Even n
pp_even (SS_Even x) = x


even_plus_even : Even n -> Even m -> Even (n + m)
even_plus_even {n = Z} Z_Even y_even = y_even
even_plus_even {n = S(S n)} (SS_Even a) y_even = SS_Even (even_plus_even a y_even)


even_times_even : Even n -> Even m -> Even (n * m)
even_times_even {n = Z} Z_Even y_even = Z_Even
even_times_even {n = S(S n)} (SS_Even a) y_even = even_plus_even y_even (even_plus_even y_even (even_times_even a y_even))

--

data LTE' : (n :Nat) -> (m:Nat) -> Type where 
    LTE_Z : LTE' Z m
    LTE_S : LTE' a b -> LTE' (S a) (S b)


lte_pred : LTE' (S n) (S m) -> LTE' n m 
lte_pred (LTE_S x) = x


lte_refl : {a : Nat} -> LTE' a a 
lte_refl {a = Z} = LTE_Z
lte_refl {a = (S k)} = LTE_S lte_refl


lte_trans : LTE' a b -> LTE' b c -> LTE' a c
lte_trans LTE_Z b = LTE_Z
lte_trans (LTE_S f) (LTE_S s) = LTE_S $ lte_trans f s


succ_larger : {n : Nat} -> LTE' n (S n)
succ_larger {n = 0} = LTE_Z
succ_larger {n = (S k)} = LTE_S $ succ_larger

lte_weaken_right : {m , n : Nat} -> LTE' m n -> LTE' m (S n)
lte_weaken_right x = lte_trans x succ_larger

lte_weaken_left : {m , n : Nat} -> LTE' (S m) n -> LTE' m n
lte_weaken_left x = lte_trans succ_larger x


zero_plus_right : (m , n : Nat) -> LTE' (m + 0) (m + n)
zero_plus_right {m = Z} {n = n}  = LTE_Z  
zero_plus_right {m = S k}  {n = n} = LTE_S $ zero_plus_right {m = k} {n = n}


zero_plus_left : (m , n : Nat) -> LTE' (0 + n) (m + n)
zero_plus_left {m = Z} {n = n} = lte_refl {a = n }
zero_plus_left {m = S k} {n = n} =  let 
    x = zero_plus_left { m = k} { n = n } in
    lte_weaken_right x


succ_plus_right : (m , n : Nat) -> LTE' (m + n) (m + S n)
succ_plus_right {m = Z} {n = n}  = succ_larger 
succ_plus_right {m = S k}  {n = n} = LTE_S $ succ_plus_right {m = k} {n = n}

succ_plus_left : (m , n : Nat) -> LTE' (m + n) (S m + n)
succ_plus_left {m = Z} {n = n}   = succ_larger 
succ_plus_left {m = S k}  {n = n} = succ_larger


data Positive : Nat -> Type where
    One_positive : Positive (S Z)
    S_positive : Positive n -> Positive (S n)



even_times_one : Even n -> Even (n * 1)
even_times_one Z_Even = Z_Even
even_times_one (SS_Even x) = SS_Even (even_times_one x)

expo : Nat -> Nat -> Nat
expo a Z = 1
expo a (S k) = a * expo a k



pow_even_pos  : Even n -> Positive m -> Even (expo n m)
pow_even_pos  a One_positive = even_times_one a  
pow_even_pos  a (S_positive x) = even_times_even a (pow_even_pos  a x)
