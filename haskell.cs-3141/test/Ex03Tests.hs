module Ex03Tests where

import Ex03
import Test.Tasty (testGroup)
import Test.Tasty.QuickCheck (testProperties, testProperty)

tests =
  testGroup "ex03" $
    [ testProperty "prop_mysteryProp_1" prop_mysteryProp_1,
      testProperty "prop_mysteryProp_2" prop_mysteryProp_2,
      testProperty "prop_mysterious_1" prop_mysterious_1,
      testProperty "prop_mysterious_2" prop_mysterious_2,
      testProperty "prop_astonishing_1" prop_astonishing_1,
      testProperty "prop_astonishing_2" prop_astonishing_2,
      testProperty "prop_astonishing_3" prop_astonishing_3
    ]