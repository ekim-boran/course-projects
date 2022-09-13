module A04_Check where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
                                         hiding ( (.&.) )
import           Test.Tasty.SmallCheck         as SC

import           Text.Printf

import           Data.Bits
import           Data.List
import           Data.Map                      as Map
import           Data.Ord
import           Data.Word

import           Control.Monad.State.Lazy

import           A02_Defs
import           A03_Check_Random_Step
import           A03_Defs
import           A04_Defs
import           A04

main :: IO ()
main = defaultMain (localOption (mkTimeout 5000000) tests)

regFile42 :: RegFile
regFile42 = RegFile $ fromList [(Reg 1, Val 2), (Reg 2, Val 0x20), (Reg 3, Val 5)]

regFile666 :: RegFile
regFile666 = RegFile $ fromList [(Reg 4, Val 6), (Reg 5, Val 7)]

memInner =
  [ (Loc $ Val 0x10 , Val 200)
  , (Loc $ Val 0x20 , Val 300)
  , (Loc $ Val 0x30 , Val 500)
  , (Loc $ Val 0x666, Val 0xffffffff)
  ]

mem :: Mem
mem = Mem $ fromList memInner

tests :: TestTree
tests = testGroup
  "A04"
  [ invalidThreadIdTests
  , randomTests
  ]

invalidThreadIdTests =
  testGroup "invalid thread id tests"
  [ testCase "invalid thread id tests 1"
  $   fromList [(42, regFile42), (666, regFile666)] |> runStateT (interpThread 37) |> flip runState mem
  @?= (((), fromList [(42, regFile42), (666, regFile666)]), Mem $ fromList $ memInner ++ [(Loc $ Val 0xdeadbeef, Val 0x22)])
  ]

randomTests =
  testGroup "random tests"
    $  randomStepCases
    |> zip [(0 :: Int) ..]
    |> fmap
         (\(i, (regFile, mem, (regFileNew, memNew))) ->
           testCase (printf "step random %02d" i)
             $   fromList [(42, regFile), (666, regFile666)] |> runStateT (interpThread 42) |> flip runState mem
             @?= (((), fromList [(42, regFileNew), (666, regFile666)]), memNew)
         )
