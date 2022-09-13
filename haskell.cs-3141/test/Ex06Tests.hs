module Ex06Tests where

import Ex06
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

tests =
  testGroup "Ex06" $
    [      
      testCase "satisfiable ex1" $ satisfiable ex1 @?= True,
      testCase "satisfiable ex2" $ satisfiable ex2 @?= True,
      testCase "satisfiable ex3" $ satisfiable ex3 @?= True,
      testCase "solutions ex1" $ solutions ex1 @?= [()],
      testCase "solutions ex1" $ solutions ex2 @?= [(1, ()), (2, ()), (3, ()), (4, ()), (5, ()), (6, ()), (7, ()), (8, ()), (9, ()), (10, ())],
      testCase "solutions ex1" $ solutions ex3 @?= [(False, (1, ())), (False, (2, ())), (True, (0, ())), (True, (1, ())), (True, (2, ()))]
    ]
