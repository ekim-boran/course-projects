module A01_Check where

import A01
import Data.List
import Data.Ord
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

-- >>> main

main :: IO ()
main = defaultMain (localOption (mkTimeout 5000000) tests)

tests :: TestTree
tests =
  testGroup
    "A01"
    [ nextWeekdayTests,
      addTupleTests,
      productDotTests,
      maybeMapTests,
      maybeThenTests,
      sumTreeTests,
      rightRotateTreeTests,
      listSumTests,
      productSeqTests,
      setMemTests,
      setEquivTests,
      setUnionTests,
      setIntersectionTests,
      setDiffTests,
      setSymDiffTests,
      relMemTests,
      relEquivTests,
      relCompTests,
      relTransTests,
      relFullTests,
      fibsTests,
      primesTests,
      fuzzySeqTests,
      funCompTests,
      curry2Tests,
      uncurry2Tests,
      myFilterTests,
      myFilterMapTests,
      myFoldLTests,
      myRevTests
    ]

nextWeekdayTests =
  testGroup
    "nextWeekDay tests"
    [ testCase "nextWeekday Sunday" $ nextWeekday Sunday @?= Monday,
      testCase "nextWeekday Monday" $ nextWeekday Monday @?= Tuesday,
      testCase "nextWeekday Tuesday" $ nextWeekday Tuesday @?= Wednesday,
      testCase "nextWeekday Wednesday" $ nextWeekday Wednesday @?= Thursday,
      testCase "nextWeekday Thursday" $ nextWeekday Thursday @?= Friday,
      testCase "nextWeekday Friday" $ nextWeekday Friday @?= Monday,
      testCase "nextWeekday Saturday" $ nextWeekday Saturday @?= Monday
    ]

addTupleTests =
  testGroup
    "addTuple Tests"
    [ testCase "addTuple 1" $ addTuple (1, 2) (3, 4) @?= (4, 6),
      testCase "addTuple 2" $ addTuple (-1, 2) (3, -4) @?= (2, -2)
    ]

productDotTests =
  testGroup
    "productDot Tests"
    [ testCase "productDot 1" $ productDot [1, 2, 3] [4, 5, 6] @?= 32,
      testCase "productDot 2" $ productDot [-1, 2, -3] [4, -5, 6] @?= -32
    ]

maybeMapTests =
  testGroup
    "maybeMap Tests"
    [ testCase "maybeMap 1" $ maybeMap (+ 1) (Just 42) @?= Just 43,
      testCase "maybeMap 2" $ maybeMap (+ 1) Nothing @?= Nothing
    ]

maybeThenTests =
  testGroup
    "maybeThen Tests"
    [ testCase "maybeThen 1" $
        maybeThen (Just 41) (\x -> if odd x then Nothing else Just (x + 1))
          @?= Nothing,
      testCase "maybeThen 2" $
        maybeThen (Just 42) (\x -> if odd x then Nothing else Just (x + 1))
          @?= Just 43,
      testCase "maybeThen 3" $
        maybeThen Nothing (\x -> if odd x then Nothing else Just (x + 1))
          @?= Nothing
    ]

sumTreeTests =
  testGroup
    "sumTree Tests"
    [ testCase "sumTree 1" $ sumTree (Leaf 42) @?= 42,
      testCase "sumTree 2" $
        sumTree (Branch 42 (Branch 37 (Leaf 132) (Leaf 137)) (Leaf 666))
          @?= 1014
    ]

rightRotateTreeTests =
  testGroup
    "rightRotateTree Tests"
    [ testCase "rightRotateTree 1" $ rightRotateTree (Leaf 42) @?= Nothing,
      testCase "rightRotateTree 2" $
        rightRotateTree (Branch 42 (Branch 37 (Leaf 132) (Leaf 137)) (Leaf 666))
          @?= Just (Branch 37 (Leaf 132) (Branch 42 (Leaf 137) (Leaf 666)))
    ]

listSumTests =
  testGroup
    "listSum Tests"
    [ testCase "listSum 1" $ listSum [] @?= 0,
      testCase "listSum 2" $ listSum [1, 2, 3] @?= 6
    ]

productSeqTests =
  testGroup
    "productSeq Tests"
    [ testCase "productSeq 1" $ productSeq (^ 2) 1 10 @?= 13168189440000,
      testCase "productSeq 2" $ productSeq (+ 1) 1 5 @?= 720
    ]

setMemTests =
  testGroup
    "setMem Tests"
    [ testCase "setMem 1" $ setMem 42 [42, 42, 42] @?= True,
      testCase "setMem 2" $ setMem 666 [42, 42, 42] @?= False,
      testCase "setMem 3" $ setMem 666 [42, 666, 137] @?= True,
      testCase "setMem 4" $ setMem 37 [42, 666, 137] @?= False
    ]

setEquivTests =
  testGroup
    "setEquiv Tests"
    [ testCase "setEquiv 1" $ setEquiv [42] [42, 42, 42] @?= True,
      testCase "setEquiv 2" $ setEquiv [666] [42, 42, 42] @?= False,
      testCase "setEquiv 3" $ setEquiv [666] [42, 666, 137] @?= False,
      testCase "setEquiv 4" $ setEquiv [137, 666, 42] [42, 666, 137] @?= True
    ]

setUnionTests =
  testGroup
    "setUnion Tests"
    [ testCase "setUnion 1" $ setEquiv (setUnion [42] [42]) [42, 42, 42] @?= True,
      testCase "setUnion 2" $
        setEquiv (setUnion [666] [42]) [666, 42, 42, 42]
          @?= True,
      testCase "setUnion 3" $
        setEquiv (setUnion [666] [666, 137]) [42, 666, 137]
          @?= False,
      testCase "setUnion 4" $
        setEquiv (setUnion [137, 666, 42] [42, 666, 137]) [42, 666, 137]
          @?= True
    ]

setIntersectionTests =
  testGroup
    "setIntersection Tests"
    [ testCase "setIntersection 1" $
        setEquiv (setIntersection [42] [42]) [42, 42, 42]
          @?= True,
      testCase "setIntersection 2" $
        setEquiv (setIntersection [666] [42]) []
          @?= True,
      testCase "setIntersection 3" $
        setEquiv (setIntersection [666] [666, 137]) [42, 666, 137]
          @?= False,
      testCase "setIntersection 4" $
        setEquiv (setIntersection [137, 666, 42] [42, 666, 137]) [42, 666, 137]
          @?= True
    ]

setDiffTests =
  testGroup
    "setDiff Tests"
    [ testCase "setDiff 1" $ setEquiv (setDiff [42] [42]) [] @?= True,
      testCase "setDiff 2" $ setEquiv (setDiff [666] [42]) [666] @?= True,
      testCase "setDiff 3" $ setEquiv (setDiff [666] [666, 137]) [] @?= True,
      testCase "setDiff 4" $
        setEquiv (setDiff [137, 666, 42] [42, 666, 137]) []
          @?= True
    ]

setSymDiffTests =
  testGroup
    "setSymDiff Tests"
    [ testCase "setSymDiff 1" $ setEquiv (setSymDiff [42] [42]) [] @?= True,
      testCase "setSymDiff 2" $
        setEquiv (setSymDiff [666] [42]) [666, 42]
          @?= True,
      testCase "setSymDiff 3" $
        setEquiv (setSymDiff [666] [666, 137]) [137]
          @?= True,
      testCase "setSymDiff 4" $
        setEquiv (setSymDiff [137, 666, 42] [42, 666, 137]) []
          @?= True
    ]

relMemTests =
  testGroup
    "relMem Tests"
    [ testCase "relMem 1" $ relMem [(42, 37), (37, 42)] 42 37 @?= True,
      testCase "relMem 2" $ relMem [(42, 37), (42, 37)] 37 42 @?= False
    ]

relEquivTests =
  testGroup
    "relEquiv Tests"
    [ testCase "relEquiv 1" $
        relEquiv [(42, 37), (37, 42)] [(37, 42), (42, 37)]
          @?= True,
      testCase "relEquiv 2" $
        relEquiv [(42, 37), (37, 42)] [(37, 42), (42, 37), (42, 37)]
          @?= True,
      testCase "relEquiv 3" $ relEquiv [(36, 666)] [(666, 36)] @?= False
    ]

relCompTests =
  testGroup
    "relComp Tests"
    [ testCase "relComp 1" $
        relEquiv (relComp [(42, 37)] [(37, 42)]) [(42, 42)]
          @?= True,
      testCase "relComp 2" $ relEquiv (relComp [(42, 37)] [(42, 37)]) [] @?= True,
      testCase "relComp 3" $
        relEquiv
          (relComp [(42, 37)] [(37, 42), (37, 666), (42, 137)])
          [(42, 42), (42, 666)]
          @?= True
    ]

relTransTests =
  testGroup
    "relTrans Tests"
    [ testCase "relTrans 1" $
        relEquiv
          (relTrans [(42, 37), (37, 137), (137, 666)])
          [(42, 37), (42, 137), (42, 666), (37, 137), (37, 666), (137, 666)]
          @?= True
    ]

relFullTests =
  testGroup
    "relFull Tests"
    [ testCase "relFull 1" $
        relEquiv
          (relFull 2)
          [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
          @?= True
    ]

fibsTests =
  testGroup
    "fibs Tests"
    [testCase "fibs 1" $ take 10 fibs @?= [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]]

primesTests =
  testGroup
    "primes Tests"
    [ testCase "primes 1" $
        take 10 primes
          @?= [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
    ]

fuzzySeqTests =
  testGroup
    "fuzzySeq Tests"
    [ testCase "fuzzySeq 1" $
        take 15 fuzzySeq
          @?= [1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 5, 4, 3, 2, 1]
    ]

funCompTests =
  testGroup
    "funComp Tests"
    [testCase "funComp 1" $ funComp (+ 1) (^ 2) 42 @?= 1849]

curry2Tests =
  testGroup
    "curry2 Tests"
    [testCase "curry2 1" $ curry2 (uncurry (+)) 42 37 @?= 79]

uncurry2Tests =
  testGroup
    "uncurry2 Tests"
    [testCase "uncurry2 1" $ uncurry2 (+) (42, 37) @?= 79]

myFilterTests =
  testGroup
    "myFilter Tests"
    [testCase "myFilter 1" $ myFilter odd [42, 36, 137, 666] @?= [137]]

myFilterMapTests =
  testGroup
    "myFilterMap Tests"
    [ testCase "myFilterMap 1" $
        myFilterMap
          (\x -> if odd x then Just (x + 1) else Nothing)
          [42, 36, 137, 666]
          @?= [138]
    ]

myFoldLTests =
  testGroup
    "myFoldL Tests"
    [ testCase "myFoldL 1" $
        myFoldL 0 (\x y -> 2 * x + y) [1, 2, 3, 4, 5, 6, 7]
          @?= 247
    ]

myRevTests =
  testGroup
    "myRev Tests"
    [testCase "myRev 1" $ myRev [1, 2, 3, 4, 5] @?= [5, 4, 3, 2, 1]]
