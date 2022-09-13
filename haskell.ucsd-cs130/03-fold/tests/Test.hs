import Test.Tasty
import Common
import Hw3

main :: IO ()
main = runTests [ unit ]

unit :: Score -> TestTree
unit sc = testGroup "Unit 2" [
  scoreTest (sqSum, [], 0, 1, "sqSum 1"),
  scoreTest (sqSum, [1,2,3,4], 30, 1, "sqSum 2"),
  scoreTest (sqSum, [-1,-2,-3,-4], 30, 1, "sqSum 3"),
  scoreTest (uncurry pipe, ([], 3), 3, 1, "pipe 1"),
  scoreTest (uncurry pipe, ([(\x-> 2*x),(\x -> x + 3)], 3), 12, 1, "pipe 2"),
  scoreTest (uncurry pipe, ([(\x -> x + 3), (\x-> 2*x)], 3), 9, 1, "pipe 3"),

  scoreTest(uncurry sepConcat, (", ",["foo","bar","baz"]), "foo, bar, baz", 1, "sepConcat 1"),
  scoreTest(uncurry sepConcat, ("---",[]), "", 1, "sepConcat 2"),
  scoreTest(uncurry sepConcat, ("",["a","b","c","d","e"]), "abcde", 1, "sepConcat 3"),
  scoreTest(uncurry sepConcat, ("X",["hello"]), "hello", 1, "sepConcat 4"),

  scoreTest(uncurry stringOfList, (intString, [1,2,3,4,5,6]), "[1, 2, 3, 4, 5, 6]",1,"stringOfList 1"),
  scoreTest(uncurry stringOfList, (id, ["foo"]), "[foo]",1,"stringOfList 2"),
  scoreTest(uncurry stringOfList, ((stringOfList intString),[[1,2,3],[4,5],[6],[]]), "[[1, 2, 3], [4, 5], [6], []]",1,"stringOfList 3"),

  scoreTest(uncurry clone, (3,5), [3,3,3,3,3],1,"clone 1"),
  scoreTest(uncurry clone, ("foo",2), ["foo","foo"],1,"clone 2"),

  scoreTest(uncurry padZero, ([9,9],[1,0,0,2]), ([0,0,9,9],[1,0,0,2]),1,"padzero 1"),
  scoreTest(uncurry padZero, ([1,0,0,2],[9,9]), ([1,0,0,2],[0,0,9,9]),1,"padzero 2"),

  scoreTest(removeZero, [0,0,0,1,0,0,2], [1,0,0,2],1,"removeZero 1"),
  scoreTest(removeZero, [9,9], [9,9],1,"removeZero 2"),

  scoreTest(normalize . uncurry bigAdd,  ([9,9],[1,0,0,2]), [1,1,0,1],1, "bigAdd 1"),
  scoreTest(normalize . uncurry bigAdd,  ([9,9,9,9],[9,9,9]), [1,0,9,9,8],1, "bigAdd 2"),

  scoreTest(normalize . uncurry mulByDigit,  (9,[9,9,9,9]), [8,9,9,9,1],1, "mulByDigit 1"),

  scoreTest(normalize . uncurry bigMul,  ([9,9,9,9],[9,9,9,9]), [9,9,9,8,0,0,0,1],1, "bigMul 1"),
  scoreTest(normalize . uncurry bigMul,  ([9,9,9,9,9],[9,9,9,9,9]), [9,9,9,9,8,0,0,0,0,1],1,"bigMul 2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
   
    normalize :: [Int] -> [Int]
    normalize xs = dropWhile (== 0) xs
