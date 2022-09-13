joinIO : IO (IO a) -> IO a
joinIO = (>>=  id)
mapIO : (a -> b) -> IO a -> IO b
mapIO f  = ( >>= (pure . f))
(>=>) : (a -> IO b) -> (b -> IO c) -> a -> IO c
(>=>) f g =  (>>= g) . f


eitherIO : Either (IO a) (IO b) -> IO (Either a b)
eitherIO (Left io) = map Left io
eitherIO (Right io) = map Right io

bothIO : Pair (IO a) (IO b) -> IO (Pair a b)
bothIO (a, b) = map (,) a <*> b -- tuple section works default


data  CoNat : Type  where
	Zero  :  CoNat
	Succ  :  (n : Inf CoNat) -> CoNat
 
implementation Num CoNat where 
    fromInteger x = if x == 0 then Zero else Succ (fromInteger (x -1))
    (+) Zero b = b
    (+) (Succ x) b = Succ (Delay (x + b))

    (*) (Succ x) (Succ y) = Succ (Delay ((x + y) + (x * y))) -- not total 
    (*) _ _  = Zero

implementation Eq CoNat where 
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b
    (==) _ _ = False
implementation Ord CoNat where 
    compare Zero Zero = EQ
    compare _ Zero = GT
    compare  Zero _ = LT
    compare (Succ x) (Succ y) = compare x y
infinity  :  CoNat
infinity  =  Succ infinity


t : CoNat -> CoNat -> CoNat 
t = (+)

data  CoList : Type -> Type  where
	Nil  :  CoList a
	(::) : (x : a) -> (xs : Inf (CoList a)) -> CoList a



implementation Cast (List a) (CoList a) where
    cast [] = Nil
    cast (x::xs) = x::(cast xs)
implementation Cast (Stream a) (CoList a) where
    cast (x::xs) = x::(cast xs)

interface Queue (queue : Type -> Type) where
    empty : queue a
    push : a -> queue a -> queue a
    pop : queue a -> Maybe (Pair a (queue a))
implementation Queue List where 
    empty = []
    push x xs = xs ++ [x]
    pop (x::xs) = Just (x, xs)
    pop [] = Nothing

data ListPair : Type -> Type where
    LP : (back : List a) -> (front : List a) -> ListPair a

implementation Queue ListPair where 
    empty = LP [] []
    push x (LP front back) = LP (x :: front) back
    pop (LP f b) = case b of 
        [] => case reverse f of
            [] => Nothing
            (x::xs) =>   Just(x, LP [] xs)
        (x::xs) => Just (x, LP f xs) 
