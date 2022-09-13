module A03_Check where

import A02_Defs
import A03
import A03_Check_Random_Instr
import A03_Check_Random_Step
import A03_Defs
import Data.Bits hiding (And, Xor)
import Data.List
import Data.Map as Map
import Data.Ord
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding
  ( (.&.),
  )
import Test.Tasty.SmallCheck as SC
import Text.Printf

main :: IO ()
main = defaultMain (localOption (mkTimeout 5000000) tests)

tests :: TestTree
tests =
  testGroup
    "A03"
    [ instrUnaryTests,
      instrBinaryTests,
      instrMemTests,
      instrJumpTests,
      stepFaultTests,
      instrRandomTests,
      stepRandomTests
    ]

regFileInner = [(Reg 1, Val 2), (Reg 2, Val 0x20), (Reg 3, Val 5)]

regFile :: RegFile
regFile = RegFile $ fromList regFileInner

memInner =
  [ (Loc $ Val 0x10, Val 200),
    (Loc $ Val 0x20, Val 300),
    (Loc $ Val 0x30, Val 500),
    (Loc $ Val 0x666, Val 0xffffffff)
  ]

mem :: Mem
mem = Mem $ fromList memInner

instrUnaryTests =
  testGroup
    "instr unary tests"
    [ testCase "instr unary tests 1" $
        runInstr regFile mem (Unary (Reg 4, Move, OperandReg (Reg 3)))
          @?= (Normal, (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val 5)], mem)),
      testCase "instr unary tests 2" $
        runInstr regFile mem (Unary (Reg 4, Negate, OperandReg (Reg 3)))
          @?= ( Normal,
                (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ negate 5)], mem)
              ),
      testCase "instr unary tests 3" $
        runInstr regFile mem (Unary (Reg 4, Complement, OperandReg (Reg 3)))
          @?= ( Normal,
                ( RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ complement 5)],
                  mem
                )
              ),
      testCase "instr unary tests 4" $
        runInstr regFile mem (Unary (Reg 4, Not, OperandReg (Reg 3)))
          @?= ( Normal,
                ( RegFile $
                    fromList $
                      regFileInner
                        ++ [(Reg 4, Val $ if 5 == 0 then 1 else 0)],
                  mem
                )
              )
    ]

instrBinaryTests =
  testGroup
    "instr binary tests"
    [ testCase "instr binary tests 1" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Add, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ 5 + 42)], mem)
              ),
      testCase "instr binary tests 2" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Sub, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ 5 - 42)], mem)
              ),
      testCase "instr binary tests 3" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Mul, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ 5 * 42)], mem)
              ),
      testCase "instr binary tests 4" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Or, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ 5 .|. 42)], mem)
              ),
      testCase "instr binary tests 5" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, And, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ 5 .&. 42)], mem)
              ),
      testCase "instr binary tests 6" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Xor, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val $ 5 `xor` 42)], mem)
              ),
      testCase "instr binary tests 7" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Lt, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                ( RegFile $
                    fromList $
                      regFileInner
                        ++ [(Reg 4, Val $ if 5 < 42 then 1 else 0)],
                  mem
                )
              ),
      testCase "instr binary tests 8" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Gt, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                ( RegFile $
                    fromList $
                      regFileInner
                        ++ [(Reg 4, Val $ if 5 > 42 then 1 else 0)],
                  mem
                )
              ),
      testCase "instr binary tests 9" $
        runInstr
          regFile
          mem
          (Binary (Reg 4, Eq, OperandReg (Reg 3), OperandVal (Val 42)))
          @?= ( Normal,
                ( RegFile $
                    fromList $
                      regFileInner
                        ++ [(Reg 4, Val $ if 5 == 42 then 1 else 0)],
                  mem
                )
              )
    ]

instrMemTests =
  testGroup
    "instr mem tests"
    [ testCase "instr mem tests load invalid" $
        runInstr regFile mem (Load (Reg 4, OperandVal (Val 0x31)))
          @?= (Fault, (regFile, Mem $ fromList $ memInner ++ [(ctrlLoc, Val 0x20)])),
      testCase "instr mem tests load" $
        runInstr regFile mem (Load (Reg 4, OperandVal (Val 0x30)))
          @?= (Normal, (RegFile $ fromList $ regFileInner ++ [(Reg 4, Val 500)], mem)),
      testCase "instr mem tests store" $
        runInstr regFile mem (Store (OperandVal (Val 0x30), OperandReg (Reg 3)))
          @?= ( Normal,
                (regFile, Mem $ fromList $ memInner ++ [(Loc $ Val 0x30, Val 5)])
              ),
      testCase "instr mem tests cas invalid" $
        runInstr
          regFile
          mem
          (Cas (Reg 1, OperandVal (Val 300), OperandVal (Val 400)))
          @?= (Fault, (regFile, Mem $ fromList $ memInner ++ [(ctrlLoc, Val 0x20)])),
      testCase "instr mem tests cas success" $
        runInstr
          regFile
          mem
          (Cas (Reg 2, OperandVal (Val 300), OperandVal (Val 400)))
          @?= ( Normal,
                ( RegFile $ fromList $ regFileInner ++ [(Reg 2, Val 300)],
                  Mem $ fromList $ memInner ++ [(Loc $ Val 0x20, Val 400)]
                )
              ),
      testCase "instr mem tests cas failure" $
        runInstr
          regFile
          mem
          (Cas (Reg 2, OperandVal (Val 301), OperandVal (Val 400)))
          @?= (Normal, (RegFile $ fromList $ regFileInner ++ [(Reg 2, Val 300)], mem))
    ]

instrJumpTests =
  testGroup
    "instr jump tests"
    [ testCase "instr jump tests jump" $
        runInstr regFile mem (Jump (OperandVal $ Val 0x42))
          @?= (JumpTo $ Loc $ Val 0x42, (regFile, mem)),
      testCase "instr jump tests condJump true" $
        runInstr
          regFile
          mem
          (CondJump (Reg 3, OperandVal $ Val 0x42, OperandVal $ Val 0x43))
          @?= (JumpTo $ Loc $ Val 0x42, (regFile, mem)),
      testCase "instr jump tests condJump false" $
        runInstr
          regFile
          mem
          (CondJump (Reg 4, OperandVal $ Val 0x42, OperandVal $ Val 0x43))
          @?= (JumpTo $ Loc $ Val 0x43, (regFile, mem))
    ]

stepFaultTests =
  testGroup
    "step fault tests"
    [ testCase "instr fault tests load" $
        runStep regFile mem
          @?= (regFile, Mem $ fromList $ memInner ++ [(Loc $ Val 0xdeadbeef, Val 0x20)]),
      testCase "instr fault tests instr" $
        runStep (RegFile $ fromList $ regFileInner ++ [(pcReg, Val 0x666)]) mem
          @?= ( RegFile $ fromList $ regFileInner ++ [(pcReg, Val 0x666)],
                Mem $ fromList $ memInner ++ [(Loc $ Val 0xdeadbeef, Val 0x21)]
              )
    ]

instrRandomTests =
  testGroup "instr random tests" $
    randomInstrCases
      |> zip [(0 :: Int) ..]
      |> fmap
        ( \(i, (regFile, mem, instr, result)) ->
            testCase (printf "instr random %02d" i) $
              runInstr regFile mem instr
                @?= result
        )

stepRandomTests =
  testGroup "step random tests" $
    randomStepCases
      |> zip [(0 :: Int) ..]
      |> fmap
        ( \(i, (regFile, mem, result)) ->
            testCase (printf "step random %02d" i) $
              runStep regFile mem
                @?= result
        )
