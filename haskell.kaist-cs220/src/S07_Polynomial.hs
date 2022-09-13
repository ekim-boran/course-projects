{-# LANGUAGE ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}

module S07_Polynomial
  () where

class Coeff c where
  czero :: c
  cone :: c
  cadd :: c -> c -> c
  cmul :: c -> c -> c

instance (Num a) => Coeff a where
  czero = 0
  cone  = 1
  cadd  = (+)
  cmul  = (*)

zipWithDefault :: a -> b -> [a] -> [b] -> [(a, b)]
zipWithDefault x y l r = go l r
 where
  go []      []      = []
  go []      r       = go [x] r
  go l       []      = go l [y]
  go (x : l) (y : r) = (x, y) : go l r

data Polynomial c = Polynomial [c]

pconst :: (Coeff c) => c -> Polynomial c
pconst coeff = Polynomial [coeff]

pvar :: (Coeff c) => Integer -> Polynomial c
pvar n = Polynomial (replicate (fromInteger n) czero ++ [cone])

padd_inner :: (Coeff c) => [c] -> [c] -> [c]
padd_inner l r = map (uncurry cadd) (zipWithDefault czero czero l r)

padd :: (Coeff c) => Polynomial c -> Polynomial c -> Polynomial c
padd (Polynomial xs) (Polynomial ys) = Polynomial (padd_inner xs ys)

pmul_inner :: (Coeff c) => [c] -> [c] -> [c]
pmul_inner [] _ = []
pmul_inner (x : xs) ys =
  padd_inner (map (cmul x) ys) (czero : pmul_inner xs ys)

pmul :: (Coeff c) => Polynomial c -> Polynomial c -> Polynomial c
pmul (Polynomial xs) (Polynomial ys) = Polynomial (pmul_inner xs ys)

instance (Coeff c) => Coeff (Polynomial c) where
  czero = pconst czero
  cone  = pconst cone
  cadd  = padd
  cmul  = pmul
