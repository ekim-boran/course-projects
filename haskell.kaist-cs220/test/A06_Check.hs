module A06_Check where

import           System.IO.Unsafe
import           System.Random

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
                                         hiding ( (.&.) )
import           Test.Tasty.SmallCheck         as SC

import           Text.Printf

import           Data.Bits
import           Data.List
import           Data.Map                      as Map
import           Data.Maybe
import           Data.Ord
import           Data.Word

import           Control.Concurrent
import           Control.Monad.State.Lazy

import           A02_Defs
import           A03_Defs
import           A05_Defs
import           A05
import           A06

regFileGen :: Word32 -> RegFile
regFileGen tid = RegFile $ fromList
  [ (pcReg, Val 0x10),
    (Reg 10, Val tid)
  ]

regFileSentinel = RegFile $ fromList [(pcReg, Val 0x20), (Reg 20, Val 0xdeadbeef)]

shrMem :: Word32 -> IO ShrMem
shrMem nthreads = do
  mem <- newShrMem
  forM_
    [ (Loc $ Val 0x10, Val $ fromJust $ encodeInstr $ Binary (Reg 11, Add, OperandReg $ Reg 10, OperandVal $ Val 1)),
      (Loc $ Val 0x11, Val $ fromJust $ encodeInstr $ Unary (Reg 12, Move, OperandVal $ Val 0x30)),
      (Loc $ Val 0x12, Val $ fromJust $ encodeInstr $ Cas (Reg 12, OperandReg $ Reg 10, OperandReg $ Reg 11)),
      (Loc $ Val 0x13, Val $ fromJust $ encodeInstr $ Binary (Reg 13, Eq, OperandReg $ Reg 10, OperandReg $ Reg 12)),
      (Loc $ Val 0x14, Val $ fromJust $ encodeInstr $ CondJump (Reg 13, OperandVal $ Val 0x15, OperandVal $ Val 0x11)),
      (Loc $ Val 0x15, Val $ fromJust $ encodeInstr $ Jump $ OperandVal $ Val 0x15),
      (Loc $ Val 0x20, Val $ fromJust $ encodeInstr $ Load (Reg 10, OperandVal $ Val 0x30)),
      (Loc $ Val 0x21, Val $ fromJust $ encodeInstr $ Binary (Reg 11, Eq, OperandReg $ Reg 10, OperandVal $ Val nthreads)),
      (Loc $ Val 0x22, Val $ fromJust $ encodeInstr $ CondJump (Reg 11, OperandVal $ Val 0x23, OperandVal $ Val 0x20)),
      (Loc $ Val 0x23, Val $ fromJust $ encodeInstr $ Store (OperandReg $ Reg 20, OperandVal $ Val 0x10)),
      (Loc $ Val 0x24, Val $ fromJust $ encodeInstr $ Jump $ OperandVal $ Val 0x24),
      (Loc $ Val 0x30, Val 0)
    ]
    (\(loc, val) -> storeShrMem loc val mem)
  return mem

casTest :: Word32 -> IO Word32
casTest nthreads = do
  let regFiles1 = fromList $ fmap (\tid -> (tid, regFileGen tid)) [0 .. (nthreads - 1)]
  let regFiles2 = Map.insert nthreads regFileSentinel regFiles1
  shrMem <- shrMem nthreads
  runStateT (runStateT interpMachine regFiles2) shrMem
  value <- loadShrMem (Loc $ Val 0x30) shrMem
  let (Val valueInner) = fromJust value
  return valueInner

main :: IO ()
main = defaultMain (localOption (mkTimeout 5000000) tests)

tests :: TestTree
tests = testGroup "A06" [casTests]

casTests = testGroup
  "cas tests"
  [ testCase "cas test 2"
  $   unsafePerformIO (casTest 2)
  @?= 2,
    testCase "cas test 8"
  $   unsafePerformIO (casTest 8)
  @?= 8,
    testCase "cas test 16"
  $   unsafePerformIO (casTest 16)
  @?= 16
  ]
