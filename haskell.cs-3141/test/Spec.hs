import Data.List (reverse)
import Ex02Tests qualified
import Ex03Tests qualified
import Ex04Tests qualified
import Ex05Tests qualified
import Ex06Tests qualified

import Hare.Tests as Hare
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Tortoise.TestSupport (MoveTurnOnly (..), NoPenControl (..))
import Tortoise.Tests as Tortoise
import Tortoise.Tortoise
import Tortoise.TortoiseCombinators

main :: IO ()
main =
  defaultMain $
    testGroup
      "all tests"
      [Ex02Tests.tests, Ex03Tests.tests, Ex04Tests.tests, Ex05Tests.tests, Ex06Tests.tests, Hare.tests, Tortoise.tests]
