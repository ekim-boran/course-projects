module Ex02Tests where

import Ex02
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit

props f =
  fmap isSuccess
    <$> sequence
      [ quickCheckWithResult (stdArgs {chatty = False}) $ sortProp1 f,
        quickCheckWithResult (stdArgs {chatty = False}) $ sortProp2 f,
        quickCheckWithResult (stdArgs {chatty = False}) $ sortProp3 f,
        quickCheckWithResult (stdArgs {chatty = False}) $ sortProp4 f,
        quickCheckWithResult (stdArgs {chatty = False}) $ sortProp5 f
      ]

tests :: TestTree
tests =
  testGroup
    "Ex02"
    [ testCase "dodgySort1" $ props dodgySort1 >>= (assertEqual "dodgySort1" [True, False, False, True, False]),
      testCase "dodgySort2" $ props dodgySort2 >>= (assertEqual "dodgySort2" [False, True, True, True, False]),
      testCase "dodgySort3" $ props dodgySort3 >>= (assertEqual "dodgySort3" [True, True, True, False, False]),
      testCase "dodgySort4" $ props dodgySort4 >>= (assertEqual "dodgySort4" [True, True, True, True, False])
    ]