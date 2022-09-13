import Test.Tasty
import Common
import Hw1 

main :: IO ()
main = runTests [unit1]

unit1 :: Score -> TestTree
unit1 sc = testGroup "Unit 1"
  [ mkTest
      sumList
      [1, 2, 3, 4]
      10
      "sumList 1"
  , mkTest
      sumList
      [1, -2, 3, 5]
      7
      "sumList 2"
  , mkTest
      sumList
      [1, 3, 5, 7, 9, 11]
      36
      "sumList 3"
  , mkTest
      digitsOfInt
      3124
      [3, 1, 2, 4]
      "digitsOfInt 1"
  , mkTest
      digitsOfInt
      352663
      [3, 5, 2, 6, 6, 3]
      "digitsOfInt 2"
  , mkTest
      digits
      31243
      [3, 1, 2, 4, 3]
      "digits 1"
  , mkTest
      digits
      (-23422)
      [2, 3, 4, 2, 2]
      "digits 2"
  , mkTest
      additivePersistence
      9876
      2
      "additivePersistence 1"
  , mkTest
      digitalRoot
      9876
      3
      "digitalRoot"
  , mkTest
      listReverse
      [1, 2, 3, 4]
      [4, 3, 2, 1]
      "reverse 1"
  , mkTest
      listReverse
      ["a", "b", "c", "d"]
      ["d", "c", "b", "a"]
      "rev 2"
  , mkTest
      palindrome
      "malayalam"
      True
      "palindrome 1"
  , mkTest
      palindrome
      "myxomatosis"
      False
      "palindrome 2"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest = mkTest' sc
