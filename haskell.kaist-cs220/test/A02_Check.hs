module A02_Check where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC

import           Data.List
import           Data.Ord
import           Data.Word

import           A02

main :: IO ()
main = defaultMain (localOption (mkTimeout 5000000) tests)

tests :: TestTree
tests = testGroup
  "A02"
  [regTests, unaryOpTests, binaryOpTests, operandTests, instrTests]

regTests = testGroup
  "reg tests"
  [ QC.testProperty "encodeDecodeReg"
                    (withMaxSuccess 10000 (\i -> encodeDecodeReg i == Just i))
  , QC.testProperty
    "decodeEncodeDecodeReg"
    (withMaxSuccess 10000 (\w -> decodeEncodeDecodeReg w == decodeReg w))
  ]

unaryOpTests = testGroup
  "unary tests"
  [ QC.testProperty
    "encodeDecodeUnaryOp"
    (withMaxSuccess 10000 (\i -> encodeDecodeUnaryOp i == Just i))
  , QC.testProperty
    "decodeEncodeDecodeUnaryOp"
    (withMaxSuccess 10000 (\w -> decodeEncodeDecodeUnaryOp w == decodeUnaryOp w)
    )
  ]

binaryOpTests = testGroup
  "binary tests"
  [ QC.testProperty
    "encodeDecodeBinaryOp"
    (withMaxSuccess 10000 (\i -> encodeDecodeBinaryOp i == Just i))
  , QC.testProperty
    "decodeEncodeDecodeBinaryOp"
    (withMaxSuccess 10000
                    (\w -> decodeEncodeDecodeBinaryOp w == decodeBinaryOp w)
    )
  ]

operandTests = testGroup
  "operand tests"
  [ QC.testProperty
    "encodeDecodeOperand"
    (withMaxSuccess 10000 (\i -> encodeDecodeOperand i == Just i))
  , QC.testProperty
    "decodeEncodeDecodeOperand"
    (withMaxSuccess 10000 (\w -> decodeEncodeDecodeOperand w == decodeOperand w)
    )
  ]

instrTests = testGroup
  "instr tests"
  [ QC.testProperty
    "encodeDecodeInstr"
    (withMaxSuccess 10000 (\i -> encodeDecodeInstr i == Just i))
  , QC.testProperty
    "decodeEncodeDecodeInstr"
    (withMaxSuccess 10000 (\w -> decodeEncodeDecodeInstr w == decodeInstr w))
  ]

encodeDecodeReg :: Reg -> Maybe Reg
encodeDecodeReg i1 = do
  w <- encodeReg i1
  decodeReg w

decodeEncodeDecodeReg :: Word32 -> Maybe Reg
decodeEncodeDecodeReg w1 = do
  i1 <- decodeReg w1
  w2 <- encodeReg i1
  decodeReg w2

encodeDecodeUnaryOp :: UnaryOp -> Maybe UnaryOp
encodeDecodeUnaryOp i1 = do
  let w = encodeUnaryOp i1
  decodeUnaryOp w

decodeEncodeDecodeUnaryOp :: Word32 -> Maybe UnaryOp
decodeEncodeDecodeUnaryOp w1 = do
  i1 <- decodeUnaryOp w1
  let w2 = encodeUnaryOp i1
  decodeUnaryOp w2

encodeDecodeBinaryOp :: BinaryOp -> Maybe BinaryOp
encodeDecodeBinaryOp i1 = do
  let w = encodeBinaryOp i1
  decodeBinaryOp w

decodeEncodeDecodeBinaryOp :: Word32 -> Maybe BinaryOp
decodeEncodeDecodeBinaryOp w1 = do
  i1 <- decodeBinaryOp w1
  let w2 = encodeBinaryOp i1
  decodeBinaryOp w2

encodeDecodeOperand :: Operand -> Maybe Operand
encodeDecodeOperand i1 = do
  w <- encodeOperand i1
  decodeOperand w

decodeEncodeDecodeOperand :: Word32 -> Maybe Operand
decodeEncodeDecodeOperand w1 = do
  i1 <- decodeOperand w1
  w2 <- encodeOperand i1
  decodeOperand w2

encodeDecodeInstr :: Instr -> Maybe Instr
encodeDecodeInstr i1 = do
  w <- encodeInstr i1
  decodeInstr w

decodeEncodeDecodeInstr :: Word32 -> Maybe Instr
decodeEncodeDecodeInstr w1 = do
  i1 <- decodeInstr w1
  w2 <- encodeInstr i1
  decodeInstr w2
