module Ex05Tests where

import Ex04
import Test.Tasty (testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests =
  testGroup "ex05" $
    [ testProperty "prop_basic" prop_basic,
      testProperty "prop_optimality" prop_optimality
    ]