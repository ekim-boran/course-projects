---------------------------------------------------------------------------

{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}

module HW4.Sat where

import Control.Applicative
import Data.Char as Char
import Data.Foldable (asum)
import Data.List as List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck

---------------------------------------------------------------------------

newtype CNF = Conj {clauses :: [Clause]} deriving (Eq, Show)

newtype Clause = Clause {lits :: [Lit]} deriving (Eq, Ord, Show)

data Lit = Lit {isPos :: Bool, var :: Var} deriving (Eq, Ord, Show)

newtype Var = Var Char deriving (Eq, Ord, Show)

vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

exampleFormula :: CNF
exampleFormula =
  Conj
    [ Clause [Lit True vA, Lit True vB, Lit True vC],
      Clause [Lit False vA],
      Clause [Lit False vB, Lit True vC]
    ]

-------------------------------------------------------------------------

neg :: Lit -> Lit
neg (Lit b x) = Lit (not b) x

instance Semigroup CNF where
  Conj c1 <> Conj c2 = Conj (c1 <> c2)

instance Monoid CNF where
  mempty = Conj mempty

instance Enum Var where
  toEnum i = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

allVars :: [Var]
allVars = [vA ..]

-------------------------------------------------------------------------

-- | The number of times each variable appears in the formula
countVars :: CNF -> Map Var Int
countVars (Conj xs) = foldr (\c m -> foldr f m (lits c)) Map.empty xs
  where
    f (Lit _ name) = Map.insertWith (+) name 1

-- | All of the variables that appear anywhere in the formula, in sorted order
vars :: CNF -> [Var]
vars = Map.keys . countVars

testCountVars :: Test
testCountVars = "countVars" ~: countVars exampleFormula ~?= Map.fromList [(vA, 2), (vB, 2), (vC, 2)]

testVars :: Test
testVars = "vars" ~: vars exampleFormula ~?= [vA, vB, vC]

-------------------------------------------------------------------------

-- >>> shrink vC
-- [Var 'A',Var 'B']
--

genVar :: Int -> Gen Var
genVar n = elements (take (abs n + 1) allVars)

genLit :: Int -> Gen Lit
genLit n = Lit <$> arbitrary <*> genVar n

genClause :: Int -> Gen Clause
genClause n = Clause <$> listOf (genLit n)

genCNF :: Int -> Gen CNF
genCNF n = Conj <$> listOf (genClause n)

defaultNumVariables :: Int
defaultNumVariables = 5

instance Arbitrary Var where
  arbitrary = genVar defaultNumVariables
  shrink v
    | v == vA = []
    | otherwise = [vA .. pred v]

instance Arbitrary Lit where
  arbitrary = genLit defaultNumVariables
  shrink (Lit b v) = map (flip Lit v) (shrink b) ++ map (Lit b) (shrink v)

instance Arbitrary Clause where
  arbitrary = genClause defaultNumVariables
  shrink (Clause l) = [Clause l' | l' <- shrink l]

instance Arbitrary CNF where
  arbitrary = fmap Conj arbitrary
  shrink (Conj x) = [Conj x' | x' <- shrink x]

---------------------------------------------------------------------
-- Satisfiable and unsatisfiable formulae

unSatFormula :: CNF
unSatFormula = Conj [Clause [Lit True vA], Clause [Lit False vA]]

-- | Assignments of values to (some) variables
type Valuation = Map Var Bool

emptyValuation :: Valuation
emptyValuation = Map.empty

fromList :: [(Var, Bool)] -> Valuation
fromList = Map.fromList

exampleValuation :: Valuation
exampleValuation = Map.fromList [(vA, False), (vB, True), (vC, True)]

litSatisfied :: Valuation -> Lit -> Bool
litSatisfied a (Lit b v) = Map.member v a && (b == a Map.! v)

satisfiedBy :: CNF -> Valuation -> Bool
satisfiedBy p a = all (any (litSatisfied a) . lits) (clauses p)

validFormula :: CNF
validFormula = Conj []

anotherUnsatFormula :: CNF
anotherUnsatFormula = Conj [Clause []]

testSatisfiedBy :: Test
testSatisfiedBy =
  "satisfiedBy"
    ~: TestList
      [ "exampleFormula" ~: assertBool "" (exampleFormula `satisfiedBy` exampleValuation),
        "another example" ~: assertBool "" (validFormula `satisfiedBy` M.empty)
      ]

prop_unSatBy :: Valuation -> Bool
prop_unSatBy v = not (unSatFormula `satisfiedBy` v)

extend :: Var -> Bool -> Valuation -> Valuation
extend = Map.insert

value :: Var -> Valuation -> Maybe Bool
value = Map.lookup

---------------------------------------------------------------------------
-- Simple SAT Solver

type Solver = CNF -> Maybe Valuation

makeValuations :: [Var] -> [Valuation]
makeValuations [] = [M.empty]
makeValuations (x : xs) = [extend x b v | v <- vals, b <- [True, False]]
  where
    vals = makeValuations xs

prop_makeValuations :: CNF -> Bool
prop_makeValuations p = length valuations == 2 ^ length ss && allElementsDistinct valuations
  where
    valuations = makeValuations ss
    ss = vars p

allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct [] = True
allElementsDistinct (x : xs) = notElem x xs && allElementsDistinct xs

-- >>>  quickCheck prop_makeValuations
-- +++ OK, passed 100 tests.
--

sat0 :: Solver
sat0 cnf = headMaybe $ filter (satisfiedBy cnf) $ makeValuations $ vars cnf
  where
    headMaybe [] = Nothing
    headMaybe (x : xs) = Just x

prop_satResultSound :: Solver -> Int -> Property
prop_satResultSound solver i =
  forAll (genCNF i) $ \p -> case solver p of
    Just a -> p `satisfiedBy` a
    Nothing -> True

unsatisfiable :: CNF -> Bool
unsatisfiable p = all (\a -> not (p `satisfiedBy` a)) (makeValuations (vars p))

prop_satResult :: Solver -> CNF -> Bool
prop_satResult solver p = case solver p of
  Just a -> p `satisfiedBy` a
  Nothing -> unsatisfiable p

test1 = quickCheck (prop_satResultSound sat0)

-- >>> :set +s
-- >>> quickCheck  (prop_satResult sat1)
-- (0.00 secs, 673,040 bytes)
-- +++ OK, passed 100 tests.
-- (0.12 secs, 105,086,720 bytes)
--

---------------------------------------------------------------------------

instantiate :: CNF -> Var -> Bool -> CNF
instantiate (Conj clauses) v bool = Conj $ mapMaybe f clauses
  where
    f (Clause xs)
      | any (\l -> v == var l && bool == isPos l) xs = Nothing
      | otherwise = Just $ Clause $ filter (\l -> v /= var l) xs

satisfiable p = isJust $ sat0 p

prop_instantiate :: CNF -> Var -> Bool
prop_instantiate p var = satisfiable p == or [satisfiable (instantiate p var b) | b <- [True, False]]

solved (Conj xs) = null xs

failed (Conj xs) = any (null . lits) xs

sat1 :: Solver
sat1 = iterate []
  where
    iterate assignments cnf
      | solved cnf = Just (fromList assignments)
      | failed cnf = Nothing
      | otherwise = asum [iterate ((i, b) : assignments) (instantiate cnf i b) | i <- take 1 $ vars cnf, b <- [True, False]]

prop_sat1 :: CNF -> Bool
prop_sat1 s = isJust (sat1 s) == isJust (sat0 s)

-- >>> :set +s
-- >>> test2

---------------------------------------------------------------------------
-- Unit propagation

-- 1) If (simplifyUnitClause s) returns Nothing, then there
--    are no remaining unit clauses in s.
-- 2) If it returns (Just s'), then s' is satisfiable iff s is.
prop_simplifyUnitClause :: CNF -> Bool
prop_simplifyUnitClause p = case simplifyUnitClause p of
  (Just (s', v, b)) -> satisfiable s' == satisfiable p
  Nothing -> True

unitClauses :: CNF -> [Lit]
unitClauses (Conj xs) = mapMaybe (\xs -> case lits xs of [x] -> Just x; _ -> Nothing) xs

simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)
simplifyUnitClause cnf = case units of
  [] -> Nothing
  (x : xs) -> Just (instantiate cnf (var x) (isPos x), var x, isPos x)
  where
    units = unitClauses cnf

sat2 :: Solver
sat2 = iterate []
  where
    iterate assignments cnf
      | solved cnf = Just (fromList assignments)
      | failed cnf = Nothing
      | Just (cnf', var, bool) <- simplifyUnitClause cnf = iterate ((var, bool) : assignments) cnf'
      | otherwise = asum [iterate ((i, b) : assignments) (instantiate cnf i b) | i <- take 1 $ vars cnf, b <- [True, False]]

prop_sat2 :: CNF -> Bool
prop_sat2 s = isJust (sat2 s) == isJust (sat0 s)

test3 = quickCheck prop_sat2

---------------------------------------------------------------------------

prop_simplifyPureLiteral :: CNF -> Bool
prop_simplifyPureLiteral p = case simplifyPureLiteral p of
  (Just (s', v, b)) -> satisfiable s' == satisfiable p
  Nothing -> True

pureLiterals :: CNF -> [(Var, Bool)]
pureLiterals (Conj xs) = [(v, a) | (v, s) <- M.assocs $ foldr (\c m -> foldr f m (lits c)) Map.empty xs, Set.size s == 1, a <- Set.toList s]
  where
    f (Lit b name) = Map.insertWith Set.union name (Set.singleton b)

simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)
simplifyPureLiteral cnf = case pureLiterals cnf of
  [] -> Nothing
  ((n, b) : xs) -> Just (instantiate cnf n b, n, b) where

-- The final DPLL algorithm:
dpll :: Solver
dpll = iterate []
  where
    iterate assignments cnf
      | solved cnf = Just (fromList assignments)
      | failed cnf = Nothing
      | Just (cnf', var, bool) <- simplifyUnitClause cnf = iterate ((var, bool) : assignments) cnf'
      | Just (cnf', var, bool) <- simplifyPureLiteral cnf = iterate ((var, bool) : assignments) cnf'
      | otherwise = asum [iterate ((i, b) : assignments) (instantiate cnf i b) | i <- take 1 $ vars cnf, b <- [True, False]]

prop_dpll :: CNF -> Bool
prop_dpll s = isJust (dpll s) == isJust (sat0 s)

test4 = quickCheck (prop_satResultSound dpll)

------------------------------------------------------------------------------
-- All the tests in one convenient place:

quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n}

main :: IO ()
main = do
  putStrLn "Unit tests:"
  runTestTT $ TestList [testCountVars, testVars, testSatisfiedBy]
  putStrLn "Quickcheck properties:"
  quickCheckN 500 prop_unSatBy
  quickCheckN 500 $ prop_satResultSound sat0 defaultNumVariables
  quickCheckN 500 $ prop_satResult sat0
  quickCheckN 500 $ prop_satResultSound sat1
  quickCheckN 500 $ prop_satResult sat1
  quickCheckN 500 $ prop_satResultSound sat2
  quickCheckN 500 $ prop_satResult sat2

  quickCheckN 500 prop_instantiate
  quickCheckN 500 prop_sat1
  quickCheckN 500 prop_simplifyUnitClause
  quickCheckN 500 prop_sat2
  quickCheckN 500 prop_simplifyPureLiteral
  quickCheckN 500 prop_dpll
  quickCheckN 500 $ prop_satResultSound dpll
  quickCheckN 500 $ prop_satResult dpll
