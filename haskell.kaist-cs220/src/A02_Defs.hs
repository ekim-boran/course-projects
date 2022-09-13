{-# LANGUAGE DeriveGeneric #-}

-- | Assignment 2: CPU core actions
module A02_Defs
  ( Reg (..),
    Val (..),
    Loc (..),
    UnaryOp (..),
    BinaryOp (..),
    Operand (..),
    Instr (..),
    encodeReg,
    encodeUnaryOp,
    encodeBinaryOp,
    encodeOperand,
    encodeInstr,
    place,
  )
where

import Data.Bits hiding (And, Xor)
import Data.Word
import GHC.Generics (Generic)
import Generic.Random
import Test.Tasty.QuickCheck as QC hiding
  ( (.&.),
  )
import Text.Printf

newtype Reg = Reg Word32 deriving (Eq, Ord, Show)

newtype Val = Val Word32 deriving (Eq, Ord, Show)

newtype Loc = Loc Val deriving (Eq, Ord, Generic)

instance Show Loc where
  show (Loc (Val val)) = printf "0x%08x" val

data UnaryOp = Move | Negate | Complement | Not deriving (Eq, Show, Generic)

data BinaryOp = Add | Sub | Mul | Or | And | Xor | Lt | Gt | Eq deriving (Eq, Show, Generic)

data Operand = OperandReg Reg | OperandVal Val deriving (Eq, Show, Generic)

-- | Instructions.
data Instr
  = Unary (Reg, UnaryOp, Operand)
  | Binary (Reg, BinaryOp, Operand, Operand)
  | Load (Reg, Operand)
  | Store (Operand, Operand)
  | Cas (Reg, Operand, Operand)
  | Jump Operand
  | CondJump (Reg, Operand, Operand)
  deriving (Eq, Show, Generic)

instance Arbitrary Reg where
  arbitrary = do
    val <- arbitrary
    return (Reg (val `rem` 32))

instance Arbitrary Val where
  arbitrary = do
    val <- arbitrary
    return (Val (val `rem` 512))

instance Arbitrary Loc where
  arbitrary = genericArbitrary uniform

instance Arbitrary UnaryOp where
  arbitrary = genericArbitrary uniform

instance Arbitrary BinaryOp where
  arbitrary = genericArbitrary uniform

instance Arbitrary Operand where
  arbitrary = genericArbitrary uniform

instance Arbitrary Instr where
  arbitrary = genericArbitrary uniform

-- | Place `w` from `from`-th LSB for `size` bits.
place :: Word32 -> Int -> Int -> Word32
place w from size = shiftL (w .&. (shiftL 1 size - 1)) from

encodeReg :: Reg -> Maybe Word32
encodeReg (Reg reg) = if reg < 32 then Just reg else Nothing

encodeUnaryOp :: UnaryOp -> Word32
encodeUnaryOp Move = 0
encodeUnaryOp Negate = 1
encodeUnaryOp Complement = 2
encodeUnaryOp Not = 3

encodeBinaryOp :: BinaryOp -> Word32
encodeBinaryOp Add = 0
encodeBinaryOp Sub = 1
encodeBinaryOp Mul = 2
encodeBinaryOp Or = 3
encodeBinaryOp And = 4
encodeBinaryOp Xor = 5
encodeBinaryOp Lt = 6
encodeBinaryOp Gt = 7
encodeBinaryOp Eq = 8

encodeOperand :: Operand -> Maybe Word32
encodeOperand (OperandReg reg) = do
  reg <- encodeReg reg
  return (place 0 0 1 .|. place reg 1 5)
encodeOperand (OperandVal (Val val)) = Just (place 1 0 1 .|. place val 1 9)

encodeUnary :: Reg -> UnaryOp -> Operand -> Maybe Word32
encodeUnary reg op src1 = do
  reg <- encodeReg reg
  let op_encoded = encodeUnaryOp op
  src1 <- encodeOperand src1
  return
    ( place 0 0 3 .|. place reg 3 5 .|. place op_encoded 8 4 .|. place src1 12 10
    )

encodeBinary :: Reg -> BinaryOp -> Operand -> Operand -> Maybe Word32
encodeBinary reg op src1 src2 = do
  reg <- encodeReg reg
  let op_encoded = encodeBinaryOp op
  src1 <- encodeOperand src1
  src2 <- encodeOperand src2
  return
    ( place 1 0 3
        .|. place reg 3 5
        .|. place op_encoded 8 4
        .|. place src1 12 10
        .|. place src2 22 10
    )

encodeLoad :: Reg -> Operand -> Maybe Word32
encodeLoad reg src1 = do
  reg <- encodeReg reg
  src1 <- encodeOperand src1
  return (place 2 0 3 .|. place reg 3 5 .|. place src1 12 10)

encodeStore :: Operand -> Operand -> Maybe Word32
encodeStore src1 src2 = do
  src1 <- encodeOperand src1
  src2 <- encodeOperand src2
  return (place 3 0 3 .|. place src1 12 10 .|. place src2 22 10)

encodeCas :: Reg -> Operand -> Operand -> Maybe Word32
encodeCas reg src1 src2 = do
  reg <- encodeReg reg
  src1 <- encodeOperand src1
  src2 <- encodeOperand src2
  return
    (place 4 0 3 .|. place reg 3 5 .|. place src1 12 10 .|. place src2 22 10)

encodeJump :: Operand -> Maybe Word32
encodeJump src1 = do
  src1 <- encodeOperand src1
  return (place 5 0 3 .|. place src1 12 10)

encodeCondJump :: Reg -> Operand -> Operand -> Maybe Word32
encodeCondJump reg src1 src2 = do
  reg <- encodeReg reg
  src1 <- encodeOperand src1
  src2 <- encodeOperand src2
  return
    (place 6 0 3 .|. place reg 3 5 .|. place src1 12 10 .|. place src2 22 10)

encodeInstr :: Instr -> Maybe Word32
encodeInstr i = case i of
  Unary (reg, op, src1) -> encodeUnary reg op src1
  Binary (reg, op, src1, src2) -> encodeBinary reg op src1 src2
  Load (reg, src1) -> encodeLoad reg src1
  Store (src1, src2) -> encodeStore src1 src2
  Cas (reg, src1, src2) -> encodeCas reg src1 src2
  Jump src1 -> encodeJump src1
  CondJump (cond, src1, src2) -> encodeCondJump cond src1 src2
