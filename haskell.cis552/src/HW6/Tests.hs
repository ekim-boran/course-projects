-- Testing code for WHILE language
-- Add your own test cases to this file
-- Make sure that you read the .lhs version for clarification
module HW6.Tests where

import Control.Applicative (Alternative (..), liftA2)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import HW6.HW6
import qualified HW6.Parser as P
import qualified HW6.ParserCombinators as P
import HW6.State (State)
import qualified HW6.State as S
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Testable (..),
    classify,
    elements,
    frequency,
    listOf,
    maxSize,
    maxSuccess,
    oneof,
    quickCheck,
    quickCheckWith,
    resize,
    scale,
    sized,
    stdArgs,
    (==>),
  )
import Text.PrettyPrint (Doc, ($$), (<+>), (<>))
import qualified Text.PrettyPrint as PP

-----------------------------------------------------------------
-- A main action to run all the tests...

main :: IO ()
main = do
  _ <-
    runTestTT
      ( TestList
          [ tExec,
            tExp,
            tStmt,
            tStmtIndent,
            tAssoc,
            tPrec,
            tTestFiles,
            tParseExp,
            tWS,
            tParseBlock,
            tParseStmt,
            tParseAssoc,
            tParsePrec,
            tParseFiles
          ]
      )
  putStrLn "Testing step exec property..."
  quickCheckN 500 prop_stepExec
  putStrLn "Testing Roundtrip property..."
  quickCheckN 500 prop_roundtrip
  return ()

------------------------- Test cases for the interpreter -----

-- >>> runTestTT tExec

tExec = TestList [tExecTest, tExecFact, tExecAbs, tExecTimes]

tExecTest :: Test
tExecTest =
  "exec wTest" ~: exec wTest Map.empty
    ~?= Map.fromList [("x", IntVal 0), ("y", IntVal 10)]

tExecFact :: Test
tExecFact =
  "exec wFact" ~: exec wFact Map.empty
    ~?= Map.fromList
      [ ("f", IntVal 120),
        ("n", IntVal 0),
        ("x", IntVal 1),
        ("z", IntVal 120)
      ]

tExecAbs :: Test
tExecAbs =
  "exec wAbs" ~: exec wAbs Map.empty
    ~?= Map.fromList [("x", IntVal 3)]

tExecTimes :: Test
tExecTimes =
  "exec wTimes" ~: exec wTimes Map.empty
    ~?= Map.fromList [("x", IntVal 0), ("y", IntVal 3), ("z", IntVal 30)]

-- Add your own test cases for the interpreter here

-------------------------- Test cases for pretty printer -----

-- Simple tests

-- >>> runTest
-- "true"
--

zeroV, oneV, twoV, threeV, trueV, falseV :: Expression
zeroV = Val (IntVal 0)
oneV = Val (IntVal 1)
twoV = Val (IntVal 2)
threeV = Val (IntVal 3)
trueV = Val (BoolVal True)
falseV = Val (BoolVal False)

tExp :: Test
tExp =
  "pp exp"
    ~: TestList
      [ oneLine oneV ~?= "1",
        oneLine (BoolVal True) ~?= "true",
        oneLine (Var "x") ~?= "x",
        oneLine (Op oneV Plus twoV) ~?= "1 + 2"
      ]

{- Note that your pretty printer should be able to take advantage of the
rendering options. For that to be true, you should never use `text` to insert
spaces or newlines---let the combinators figure that out for you! -}

skip :: Block
skip = Block []

a =
  writeFile "./a.txt" $ PP.render $ pp (wTest)

-- >>> runTestTT tPrec

tStmt =
  "pp stmt oneLine"
    ~: TestList
      [ oneLine (Assign "x" threeV) ~?= "x = 3;",
        oneLine (Block []) ~?= "",
        oneLine (If (Val (BoolVal True)) skip skip)
          ~?= "if (true) { } else { }",
        oneLine (While (Val (BoolVal True)) skip)
          ~?= "while (true) { }",
        oneLine wFact ~?= "n = 5; f = 1; while (n > 0) { x = n; z = f; while (x > 1) { f = z + f; x = x - 1; } n = n - 1; }"
      ]

tStmtIndent :: Test
tStmtIndent =
  "pp stmt indent"
    ~: TestList
      [ indented (If (Val (BoolVal True)) (Block []) (Block []))
          ~?= "if (true) {\n} else {\n}",
        indented (While (Val (BoolVal True)) (Block []))
          ~?= "while (true) {\n}"
      ]

tTestFiles :: Test
tTestFiles =
  "pp test files"
    ~: TestList
      [ "fact" ~: p "fact.imp" wFact,
        "test" ~: p "test.imp" wTest,
        "abs" ~: p "abs.imp" wAbs,
        "times" ~: p "times.imp" wTimes
      ]
  where
    p fn ast = do
      str <- readFile ("./data/HW6/" ++ fn)
      assert (str == indented ast)

{- Furthermore, your pretty printer must take precedence and associativity into
account and produce output with the *smallest* number of parentheses. -}

tAssoc :: Test
tAssoc =
  "pp assoc"
    ~: TestList
      [ oneLine (Op oneV Plus (Op twoV Plus threeV)) ~?= "1 + (2 + 3)",
        oneLine (Op (Op oneV Plus twoV) Plus threeV) ~?= "1 + 2 + 3",
        oneLine (Op oneV Times (Op twoV Times threeV)) ~?= "1 * (2 * 3)",
        oneLine (Op (Op oneV Times twoV) Times threeV) ~?= "1 * 2 * 3",
        oneLine (Op (Op oneV Minus twoV) Plus threeV) ~?= "1 - 2 + 3",
        oneLine (Op oneV Plus (Op twoV Minus threeV)) ~?= "1 + (2 - 3)"
      ]

tPrec :: Test
tPrec =
  "pp prec"
    ~: TestList
      [ oneLine (Op oneV Times (Op twoV Plus threeV)) ~?= "1 * (2 + 3)",
        oneLine (Op (Op oneV Plus twoV) Times threeV) ~?= "(1 + 2) * 3",
        oneLine (Op oneV Plus (Op twoV Times threeV)) ~?= "1 + 2 * 3",
        oneLine (Op (Op oneV Times twoV) Plus threeV) ~?= "1 * 2 + 3"
      ]

-- Add your own test cases for the pretty printer here

-------------------------- Test cases for stepping ----------------------------

-- >>> runTestTT tParseFiles
-- Cases: 4  Tried: 0  Errors: 0  Failures: 0
--
-- ### Error in:   parse files:0:fact
-- fact.imp: openFile: does not exist (No such file or directory)
-- <BLANKLINE>
-- Cases: 4  Tried: 1  Errors: 1  Failures: 0
--
-- ### Error in:   parse files:1:test
-- test.imp: openFile: does not exist (No such file or directory)
-- <BLANKLINE>
-- Cases: 4  Tried: 2  Errors: 2  Failures: 0
--
-- ### Error in:   parse files:2:abs
-- abs.imp: openFile: does not exist (No such file or directory)
-- <BLANKLINE>
-- Cases: 4  Tried: 3  Errors: 3  Failures: 0
--
-- ### Error in:   parse files:3:times
-- times.imp: openFile: does not exist (No such file or directory)
-- Cases: 4  Tried: 4  Errors: 4  Failures: 0
-- Counts {cases = 4, tried = 4, errors = 4, failures = 0}
--
-- Cases: 4  Tried: 4  Errors: 0  Failures: 0
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}
--

tStepper :: Test
tStepper = TestList [tStep, tStepFiles]

tStepFiles :: Test
tStepFiles = TestList [tExecStepTest, tExecStepFact, tExecStepAbs, tExecStepTimes]

tStep :: Test
tStep =
  "step"
    ~: TestList
      [ test (Block [ax3, ax2]) (Block [ax2], m3),
        test (Block [w1]) (Block [ax3, w1], Map.empty),
        test (Block [i1]) (Block [ax3], Map.empty),
        test (Block [ax3]) (Block [], m3)
      ]
  where
    test b1 r2 = S.runState (step b1) Map.empty ~?= r2
    w1 = While (Op oneV Gt zeroV) (Block [ax3])
    i1 = If (Op oneV Gt zeroV) (Block [ax3]) skip
    ax2 = Assign "x" twoV
    ax3 = Assign "x" threeV
    m3 = Map.fromList [("x", IntVal 3)]

tExecStepTest :: Test
tExecStepTest =
  "execStep wTest" ~: execStep wTest Map.empty
    ~?= Map.fromList [("x", IntVal 0), ("y", IntVal 10)]

tExecStepFact :: Test
tExecStepFact =
  "execStep wFact" ~: execStep wFact Map.empty
    ~?= Map.fromList [("f", IntVal 120), ("n", IntVal 0), ("x", IntVal 1), ("z", IntVal 120)]

tExecStepAbs :: Test
tExecStepAbs =
  "execStep wAbs" ~: execStep wAbs Map.empty
    ~?= Map.fromList [("x", IntVal 3)]

tExecStepTimes :: Test
tExecStepTimes =
  "execStep wTimes" ~: execStep wTimes Map.empty
    ~?= Map.fromList [("x", IntVal 0), ("y", IntVal 3), ("z", IntVal 30)]

-------------------------- Evaluation simulation property ---------------------

{- Our stepper-based evaluator should agree with the original evaluator that we defined
above.  However, note that to use quickcheck, we should only compare programs that
actually terminate within a reasonable number of steps. -}

prop_stepExec b =
  not (final b) ==> final b1 ==> m1 == m2
  where
    (b1, m1) = S.runState (boundedStep 100 b) Map.empty
    m2 = exec b Map.empty

-- We can test this property using quickcheck.

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n, maxSize = 100}

-- >>> quickCheckN  1000 prop_stepExec
-- +++ OK, passed 1000 tests; 453 discarded.
--

-------------------------- Test cases for parsing -----------------------------

allTests = runTestTT $ TestList [tParseExp, tParseAssoc, tParsePrec, tWS, tParseStmt, tParseBlock, tParseFiles]

-- >>> allTests

-- >>>  quickCheck prop_roundtrip

-- *** Failed! Falsified (after 19 tests):

-- If (Op (Var "o") Ge (Val (IntVal (-12)))) (Block [If (Var "s") (Block [If (Var "r") (Block []) (Block []),Assign "w" (Val (BoolVal True))]) (Block []),Assign "e" (Var "e"),If (Var "z") (Block []) (Block []),Assign "w" (Var "w")]) (Block [Assign "v" (Op (Op (Var "x") Divide (Var "y")) Times (Var "l")),Assign "o" (Op (Var "v") Gt (Var "t"))])
--

tParseExp :: Test
tParseExp =
  "parse exp"
    ~: TestList
      [ "1" ~: P.parse exprP "1" ~?= Right (Val (IntVal 1)),
        "1 + 2" ~: P.parse exprP "1 + 2" ~?= Right (Op (Val (IntVal 1)) Plus (Val (IntVal 2))),
        "x" ~: P.parse exprP "x" ~?= Right (Var "x"),
        "true" ~: P.parse exprP "true" ~?= Right (Val (BoolVal True))
      ]

tParseAssoc :: Test
tParseAssoc =
  "parse assoc"
    ~: TestList
      [ pt (Op oneV Plus (Op twoV Plus threeV)) "1 + (2 + 3)",
        pt (Op (Op oneV Plus twoV) Plus threeV) "1 + 2 + 3",
        pt (Op oneV Times (Op twoV Times threeV)) "1 * (2 * 3)",
        pt (Op (Op oneV Times twoV) Times threeV) "1 * 2 * 3",
        pt (Op (Op oneV Minus twoV) Plus threeV) "1 - 2 + 3",
        pt (Op oneV Plus (Op twoV Minus threeV)) "1 + (2 - 3)"
      ]
  where
    pt ast str =
      str ~: P.parse exprP str ~?= Right ast

tParsePrec :: Test
tParsePrec =
  "parse prec"
    ~: TestList
      [ pt (Op oneV Times (Op twoV Plus threeV)) "1 * (2 + 3)",
        pt (Op (Op oneV Plus twoV) Times threeV) "(1 + 2) * 3",
        pt (Op oneV Plus (Op twoV Times threeV)) "1 + 2 * 3",
        pt (Op (Op oneV Times twoV) Plus threeV) "1 * 2 + 3"
      ]
  where
    pt ast str =
      str ~: P.parse exprP str ~?= Right ast

tWS :: Test
tWS =
  "parse ws"
    ~: TestList
      [ "1 " ~: P.parse exprP "1 " ~?= Right (Val (IntVal 1)),
        "1  + 2" ~: P.parse exprP "1  + 2" ~?= Right (Op (Val (IntVal 1)) Plus (Val (IntVal 2))),
        "1+2" ~: P.parse exprP "1+2" ~?= Right (Op (Val (IntVal 1)) Plus (Val (IntVal 2))),
        "x=3;" ~: P.parse statementP "x=3;" ~?= Right (Assign "x" (Val (IntVal 3)))
      ]

tParseStmt :: Test
tParseStmt =
  "parse statement"
    ~: TestList
      [ pt "x = 2;" (Assign "x" (Val (IntVal 2))),
        pt "if (true) {} else {}" (If (Val (BoolVal True)) (Block []) (Block [])),
        pt "while (true) {}" (While (Val (BoolVal True)) (Block []))
      ]
  where
    pt str ast =
      str ~: P.parse statementP str ~?= Right ast

tParseBlock :: Test
tParseBlock =
  "parse block"
    ~: TestList
      [ pt "" (Block []),
        pt "x = 2;" (Block [Assign "x" (Val (IntVal 2))])
      ]
  where
    pt str ast =
      str ~: P.parse toplevelP str ~?= Right ast

tParseFiles :: Test
tParseFiles =
  "parse files"
    ~: TestList
      [ "fact" ~: p "fact.imp" wFact,
        "test" ~: p "test.imp" wTest,
        "abs" ~: p "abs.imp" wAbs,
        "times" ~: p "times.imp" wTimes
      ]
  where
    p fn ast = do
      result <- P.parseFromFile (const <$> toplevelP <*> P.eof) ("./data/HW6/" ++ fn)
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

-- Add your own test cases for the parser here

-------------------------- Round trip property -----------------------------

{- Finally, not only should you be able to parse all of the test cases, but your
parser and pretty printer should also satisfy the following "round tripping"
property: pretty printing a WHILE program followed by parsing should return to
the original program.  (The other direction will not necessarily hold because
of whitespace.) -}

prop_roundtrip :: Expression  -> Bool
prop_roundtrip s =  P.parse exprP  (indented s) == Right s

-- >>> P.parse exprP ("1 + 1 + 1")
-- Right (Op (Op (Val (IntVal 1)) Plus (Val (IntVal 1))) Plus (Val (IntVal 1)))
--

-------------------------- Arbitrary instances -----------------------------

-- As usual with QuickCheck, we need an `Arbitrary` instance for WHILE programs.
-- We've provided these instances for you

arbVar :: Gen Variable
arbVar = elements $ map (: []) ['a' .. 'z']

genExp :: Int -> Gen Expression
genExp 0 = oneof [Var <$> arbVar, Val <$> arbitrary]
genExp n =
  frequency
    [ (1, Var <$> arbVar),
      (1, Val <$> arbitrary),
      (n, Op <$> genExp n' <*> arbitrary <*> genExp n')
    ]
  where
    n' = n `div` 2

genStatement :: Int -> Gen Statement
genStatement 0 = Assign <$> arbVar <*> genExp 0
genStatement n =
  frequency
    [ (1, Assign <$> arbVar <*> genExp n'),
      (n, If <$> genExp n' <*> genBlock n' <*> genBlock n'),
      (n, While <$> genExp n' <*> genBlock n')
    ]
  where
    n' = n `div` 2

genBlock :: Int -> Gen Block
genBlock n = Block <$> genStmts n
  where
    genStmts 0 = pure []
    genStmts n =
      frequency
        [ (1, return []),
          (n, (:) <$> genStatement n' <*> genStmts n')
        ]
      where
        n' = n `div` 2

instance Arbitrary Statement where
  arbitrary = sized genStatement

  shrink (Assign v e) = [Assign v e' | e' <- shrink e]
  shrink (If v e1 e2) =
    [ If v' e1' e2' | v' <- shrink v, e1' <- shrink e1, e2' <- shrink e2
    ]
  shrink (While c e) =
    [ While c' e' | c' <- shrink c, e' <- shrink e
    ]

instance Arbitrary Block where
  arbitrary = sized genBlock
  shrink (Block ss) = [Block ss' | ss' <- shrink ss]

instance Arbitrary Expression where
  arbitrary = sized genExp

  shrink (Op e1 o e2) = [Op e1' o e2' | e1' <- shrink e1, e2' <- shrink e2]
  shrink _ = []

instance Arbitrary Bop where
  arbitrary = elements [Plus, Minus, Times, Divide, Gt, Ge, Lt, Le]

instance Arbitrary Value where
  arbitrary = oneof [IntVal <$> arbitrary, BoolVal <$> arbitrary]
