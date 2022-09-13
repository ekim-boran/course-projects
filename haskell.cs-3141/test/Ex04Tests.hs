module Ex04Tests where

import Ex05
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

tests =
  testGroup "ex03" $
    [ testCase "3 4 +" $ calculate "3 4 +" @?= (Just 7),
      testCase "3 4 - 5 +" $ calculate "3 4 - 5 +" @?= (Just 4),
      testCase "3 4 2 / *" $ calculate "3 4 2 / *" @?= (Just 6),
      testCase "3 4 +" $ calculate "3 4 2 / * +" @?= Nothing
    ]
 