{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf)
import qualified Language.Nano.Types   as Nano
import qualified Language.Nano.Eval    as Nano
import qualified Data.BST              as BST
import qualified Test.Tasty.QuickCheck as QC
import           System.FilePath
import           GHC.IO.Encoding

main :: IO ()
main = setLocaleEncoding utf8 >> runTests 
  [ bst_props  -- 85
  , nano_eval  -- 55 
  , nano_exn   -- 60 
  , nano_repl  -- 55
  ]

-------------------------------------------------------------------------------
-- | Nano-Repl
-------------------------------------------------------------------------------
nano_repl :: Score -> TestTree
nano_repl sc = testGroup "Repl"
 [ bc "tests/input/sh0.cmd"  5 "repl-0" 
 , bc "tests/input/sh1.cmd"  5 "repl-1" 
 , bc "tests/input/sh2.cmd" 10 "repl-2" 
 , bc "tests/input/sh3.cmd"  5 "repl-3" 
 , bc "tests/input/sh4.cmd"  5 "repl-4" 
 , bc "tests/input/sh5.cmd" 10 "repl-5" 
 , bc "tests/input/sh6.cmd" 15 "repl-6" 
 ]
 where 
  bc f pts name = binTest sc (BinCmd "stack run --allow-different-user" f (f <.> "out") pts name)

-------------------------------------------------------------------------------
-- | Nano-Exceptions
-------------------------------------------------------------------------------


nano_exn :: Score -> TestTree
nano_exn sc = testGroup "Exceptions" 
  [ scoreTest ( nanoExec, ex_1_2         , Nano.VInt 3, 5, "exn - 1")
  , scoreTest ( nanoExec, ex_t1_2        , Nano.VInt 1, 5, "exn - 2")
  , scoreTest ( nanoExec, ex_1_t2        , Nano.VInt 2, 5, "exn - 3")
  , scoreTest ( nanoExec, ex_t1_t2       , Nano.VInt 1, 5, "exn - 4")
  , scoreTest ( nanoExec, ex_t12         , Nano.VInt 3, 5, "exn - 5")
  , scoreTest ( nanoExec, ex_tt12        , Nano.VInt 2, 5, "exn - 6")

  , scoreTest ( nanoExec, exTry ex_1_2   , Nano.VInt 3 , 5, "exn - 1")
  , scoreTest ( nanoExec, exTry ex_t1_2  , Nano.VInt 11, 5, "exn - 2")
  , scoreTest ( nanoExec, exTry ex_1_t2  , Nano.VInt 12, 5, "exn - 3")
  , scoreTest ( nanoExec, exTry ex_t1_t2 , Nano.VInt 11, 5, "exn - 4")
  , scoreTest ( nanoExec, exTry ex_t12   , Nano.VInt 13, 5, "exn - 5")
  , scoreTest ( nanoExec, exTry ex_tt12  , Nano.VInt 12, 5, "exn - 6")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest = scoreTest_ sc
    nanoExec  = Nano.eval []
    ex_1_2    = Nano.EBin Nano.Plus (Nano.EInt 1) (Nano.EInt 2)
    ex_t1_2   = Nano.EBin Nano.Plus (Nano.EThr (Nano.EInt 1)) (Nano.EInt 2)
    ex_1_t2   = Nano.EBin Nano.Plus (Nano.EInt 1) (Nano.EThr (Nano.EInt 2)) 
    ex_t1_t2  = Nano.EBin Nano.Plus (Nano.EThr (Nano.EInt 1)) (Nano.EThr (Nano.EInt 2)) 
    ex_t12    = Nano.EThr (Nano.EBin Nano.Plus (Nano.EInt 1)  (Nano.EInt 2))
    ex_tt12   = Nano.EThr (Nano.EBin Nano.Plus (Nano.EInt 1)  (Nano.EThr (Nano.EInt 2)))
    exTry e   = Nano.ETry e "z" (Nano.EBin Nano.Plus "z" (Nano.EInt 10))


-------------------------------------------------------------------------------
-- | Nano-Eval
-------------------------------------------------------------------------------
nano_eval :: Score -> TestTree
nano_eval sc = testGroup "Eval"
  [ scoreTest ( Nano.eval env1
              , Nano.EVar "c1"
              , Nano.VInt 1
              , 1
              , "1a - c1")
  , scoreTest ( Nano.eval env1
              , Nano.EBin Nano.Mul
                  ( Nano.EBin Nano.Minus (Nano.EInt 20) "c1" )
                  ( Nano.EBin Nano.Plus  "c2" "c3")
              , Nano.VInt 95
              , 3
              , "1a - (20-c1)*(c2+c3)")
  , failTest  ( Nano.eval env2
              , Nano.EBin Nano.Plus "bt" "c3"
              , "type error"
              , 1
              , "1b - True + 3")
  , failTest  ( Nano.eval env2
              , Nano.EBin Nano.Or "bt" "c3"
              , "type error"
              , 1
              , "1b - bt||c3")
  , scoreTest ( Nano.eval env2
              , Nano.EIf (Nano.EBin Nano.Lt (Nano.EInt 3) (Nano.EInt 3))
                (Nano.EInt 2) (Nano.EInt 4)
              , Nano.VInt 4
              , 1
              , "1b - if (3 < 3) then 2 else 4")
  , scoreTest ( Nano.eval []
              , Nano.ELet "x" (Nano.EInt 4) "x"
              , Nano.VInt 4
              , 1
              , "1c - let x = 4 in x")
  , scoreTest ( Nano.eval []
              , Nano.EApp (Nano.ELam "x" (Nano.EBin Nano.Mul "x" "x")) (Nano.EInt 5)
              , Nano.VInt 25
              , 2
              , "1d - (\\x -> x*x) 5")
  , scoreTest ( Nano.eval []
              , (Nano.ELet "fac"
                  (Nano.ELam "n"
                    (Nano.EIf (Nano.EBin Nano.Eq "n" (Nano.EInt 0))
                      (Nano.EInt 1)
                      (Nano.EBin Nano.Mul "n"
                        (Nano.EApp "fac"
                          (Nano.EBin Nano.Minus "n" (Nano.EInt 1))))))
                  (Nano.EApp "fac" (Nano.EInt 10)))
              , Nano.VInt 3628800
              , 1
              , "1e - let fac = (\\ n -> if n == 0 then 1 else n * fac (n - 1)) in fac 10")
  , scoreTest ( Nano.eval Nano.prelude
              , (Nano.EApp "head" (Nano.EBin Nano.Cons (Nano.EInt 1) Nano.ENil))
              , Nano.VInt 1
              , 1
              , "1f - head (1 , nil)")
  , scoreTest ( Nano.eval Nano.prelude
              , (Nano.EApp "tail" (Nano.EBin Nano.Cons (Nano.EInt 1) Nano.ENil))
              , Nano.VNil
              , 1
              , "1f - tail (1 , nil)")
  , scoreTest ( parse
              , "True"
              , Nano.EBool True
              , 1
              , "2a - \"true\"")
  , scoreTest ( parse
              , "123\n\t"
              , Nano.EInt 123
              , 1
              , "2a - \" 894\\n\\t\"")
  , scoreTest ( parse
              , "x"
              , Nano.EVar "x"
              , 1
              , "2a - \"Z\"")
  , scoreTest ( parse
              , "let x = 5 in x"
              , Nano.ELet "x" (Nano.EInt 5) (Nano.EVar "x")
              , 1
              , "2b - let x = 5 in x")
  , scoreTest ( parse
              , "\\x -> 5"
              , Nano.ELam "x" (Nano.EInt 5)
              , 1
              , "2b - \\x -> 5")
  , scoreTest ( parse
              , "if a then b else c"
              , Nano.EIf (Nano.EVar "a") (Nano.EVar "b") (Nano.EVar "c")
              , 1
              , "2b - if a then b else c")
  , scoreTest ( parse
              , "x + 2"
              ,  Nano.EBin Nano.Plus (Nano.EVar "x") (Nano.EInt 2)
              , 1
              , "2c - x+2")
  , scoreTest ( parse
              , "x <= 2"
              , Nano.EBin Nano.Le (Nano.EVar "x") (Nano.EInt 2)
              , 1
              , "2c - x<=2")
  , scoreTest ( parse
              , "x && 2"
              , Nano.EBin Nano.And (Nano.EVar "x") (Nano.EInt 2)
              , 1
              , "2c - x&&2")
  , scoreTest ( parse
              , "1 + ( 2 * ( 3 ))"
              , Nano.EBin Nano.Plus (Nano.EInt 1) (Nano.EBin Nano.Mul (Nano.EInt 2) (Nano.EInt 3))
              , 1
              , "2d - 1+(2*(3))")
  , scoreTest ( parse
              , "f x"
              , Nano.EApp (Nano.EVar "f") (Nano.EVar "x")
              , 1
              , "2d - f x")
  , scoreTest ( parse
              , "1+a&&b||c+d*e-f-g x"
              , Nano.EBin Nano.Or (Nano.EBin Nano.And
                                   (Nano.EBin Nano.Plus (Nano.EInt 1) (Nano.EVar "a"))
                                   (Nano.EVar "b"))
                (Nano.EBin Nano.Minus (Nano.EBin Nano.Minus
                                       (Nano.EBin Nano.Plus (Nano.EVar "c")
                                        (Nano.EBin Nano.Mul (Nano.EVar "d")
                                         (Nano.EVar "e")))
                                        (Nano.EVar "f"))
                  (Nano.EApp (Nano.EVar "g") (Nano.EVar "x")))
              , 1
              , "2e - 1+a&&b||c+d*e-f-g x")

  , scoreTest ( parse
              , "e : f"
              , Nano.EBin Nano.Cons (Nano.EVar "e") (Nano.EVar "f")
              , 1
              , "2f - e:f")

  , fileTest  ( "tests/input/t1.hs"
              , Nano.VInt 45
              , 1 )
  , fileTest  ( "tests/input/t2.hs"
              , Nano.VInt 0
              , 1 )
  , fileTest  ( "tests/input/t3.hs"
              , Nano.VInt 2
              , 1 )
  , fileTestE ( "tests/input/t4.hs"
              , "bound"
              , 1 )
  , fileTest  ( "tests/input/t5.hs"
              , Nano.VInt 6
              , 1 )
  , fileTest  ( "tests/input/t6.hs"
              , Nano.VInt 102
              , 2 )
  , fileTest  ( "tests/input/t8.hs"
              , Nano.VInt 55
              , 2 )
  , fileTest  ( "tests/input/t9.hs"
              , Nano.VInt 3628800
              , 2 )
  , fileTest  ( "tests/input/t10.hs"
              , Nano.VInt 110
              , 2 )
  , fileTest  ( "tests/input/t11.hs"
              , Nano.VInt 55
              , 2 )
  , fileTest  ( "tests/input/t12.hs"
              , Nano.VInt 3628800
              , 2 )
  , fileTest  ( "tests/input/t13.hs"
              , Nano.VInt 80
              , 2 )
  , fileTest  ( "tests/input/t14.hs"
              , Nano.valueList [Nano.VInt 1, Nano.VInt 6, Nano.VInt 7, Nano.VInt 8]
              , 2 )
  , fileTest  ( "tests/input/t15.hs"
              , Nano.VBool False
              , 2 )
  , fileTest  ( "tests/input/t16.hs"
              , Nano.valueList [Nano.VInt 2, Nano.VInt 3, Nano.VInt 4, Nano.VInt 5]
              , 3 )
  , fileTest  ( "tests/input/t17.hs"
              , Nano.VInt 10
              , 3 )
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest = scoreTest_ sc
    failTest  = failTest_ sc
    fileTest  = fileTest_ sc
    fileTestE = fileTestE_ sc
    parse     = Nano.parse

scoreTest_ :: (Show b, Eq b) => Score -> (a -> b, a, b, Int, String) -> TestTree
scoreTest_ sc (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

failTest_ :: (Show b, Eq b) => Score -> (a -> b, a, String, Int, String) -> TestTree
failTest_ sc (f, x, err, n, msg) = scoreTest' sc (expectError err (return . f), x, True, n, msg)

fileTest_ sc (f, r, n)  = scoreTest' sc (Nano.execFile, f, r, n, "file: " ++ f)
fileTestE_ sc (f, e, n) = scoreTest' sc (expectError e Nano.execFile, f, True, n, "file: " ++ f)


expectError :: (Show b) => String -> (a -> IO b) -> a -> IO Bool
expectError err f x = do { r <- f x; print r; return False }
                      `catch`
                      (return . isInfixOf err . Nano.errMsg)

env1 :: Nano.Env
env1 =
  [ ("c0", Nano.VInt 0)
  , ("c1", Nano.VInt 1)
  , ("c2", Nano.VInt 2)
  , ("c3", Nano.VInt 3)
  , ("c0", Nano.VInt 4)
  , ("c1", Nano.VInt 5)
  ]

env2 :: Nano.Env
env2 = env1 ++
  [ ("bt", Nano.VBool True)
  , ("bf", Nano.VBool False)
  ]

-------------------------------------------------------------------------------
-- | BST ----------------------------------------------------------------------
-------------------------------------------------------------------------------
bst_props :: Score -> TestTree
bst_props sc = testGroup "BinarySearchTree"
  [ scoreProp sc ("prop_build"        , BST.prop_build        , 10) 
  , scoreProp sc ("prop_contains_elt" , BST.prop_contains_elt , 10)
  , scoreProp sc ("prop_contains_elts", BST.prop_contains_elts, 15)
  , scoreProp sc ("prop_add_elt"      , BST.prop_add_elt      , 5)
  , scoreProp sc ("prop_add_elts_old" , BST.prop_add_elts_old , 5)
  , scoreProp sc ("prop_add_isOrd"    , BST.prop_add_isOrd    , 5)
  , scoreProp sc ("prop_multiset"     , BST.prop_multiset     , 5)
  , scoreProp sc ("prop_remove_min"   , BST.prop_remove_min   , 10)
  , scoreProp sc ("prop_remove"       , BST.prop_remove       , 7)
  , scoreProp sc ("prop_remove_old"   , BST.prop_remove_old   , 8)
  , scoreProp sc ("prop_remove_isOrd" , BST.prop_remove_isOrd , 5)
  ]
