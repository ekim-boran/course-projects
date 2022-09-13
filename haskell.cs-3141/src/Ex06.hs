{-# LANGUAGE GADTs #-}

module Ex06 where


-- Datatype of formulas
-- --------------------

data Formula ts where
  Body   ::Term Bool                     -> Formula ()
  Exists ::Show a
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Name    ::String -> Term t    -- to facilitate pretty printing only. 
                                 -- don't use this in actual formulae.

  Con     ::t -> Term t -- Constant values

  -- Logical operators
  And     ::Term Bool -> Term Bool -> Term Bool
  Or      ::Term Bool -> Term Bool -> Term Bool

  -- Comparison operators
  Smaller ::Term Int  -> Term Int  -> Term Bool

  -- Arithmetic operators
  Plus    ::Term Int  -> Term Int  -> Term Int


-- Pretty printing formulas
-- ------------------------

instance Show t => Show (Term t) where
  show (Con v      ) = show v
  show (And     p q) = "(" ++ show p ++ " && " ++ show q ++ ")"
  show (Or      p q) = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (Smaller n m) = "(" ++ show n ++ " < " ++ show m ++ ")"
  show (Plus    n m) = "(" ++ show n ++ " + " ++ show m ++ ")"
  show (Name name  ) = name

instance Show (Formula ts) where
  show = show' [ 'x' : show i | i <- [0 ..] ]
   where
    show' :: [String] -> Formula ts' -> String
    show' ns (Body body) = show body
    show' (n : ns) (Exists vs p) =
      "exists " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))


-- Example formulas
-- ----------------

ex1 :: Formula ()
ex1 = Body (Con True)

ex2 :: Formula (Int, ())
ex2 = Exists [1 .. 10] $ \n -> Body $ n `Smaller` (n `Plus` Con 1)

ex3 :: Formula (Bool, (Int, ()))
ex3 = Exists [False, True]
  $ \p -> Exists [0 .. 2] $ \n -> Body $ p `Or` (Con 0 `Smaller` n)

-- Evaluating terms
-- ----------------
eval :: Term t -> t
eval (Con a      ) = a
eval (Smaller l r) = eval l < eval r
eval (Plus    l r) = eval l + eval r
eval (And     l r) = eval l && eval r
eval (Or      l r) = eval l || eval r
eval (Name _     ) = error "eval: Name"


-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable (Body x) = eval x
satisfiable (Exists xs f) =
  foldr (\x xs -> satisfiable (f (Con x)) || xs) True xs


-- >>> satisfiable ex1
-- >>> satisfiable  ex2
-- >>> satisfiable  ex3
-- True
-- True
-- True
--

-- >>> solutions ex1
-- >>> solutions  ex2
-- >>> solutions  ex3
-- [()]
-- [(1,()),(2,()),(3,()),(4,()),(5,()),(6,()),(7,()),(8,()),(9,()),(10,())]
-- [(False,(1,())),(False,(2,())),(True,(0,())),(True,(1,())),(True,(2,()))]
--

-- Enumerating solutions of formulae
-- ---------------------------------

solutions :: Formula ts -> [ts]
solutions (Body x) = [() | eval x]
solutions (Exists xs f) =
  [ (x, rest) | x <- xs, rest <- solutions $ f (Con x) ]


