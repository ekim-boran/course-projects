-- | Assignment 2: CPU core actions
module A02
  ( Reg,
    UnaryOp,
    BinaryOp,
    Operand,
    Instr,
    encodeReg,
    encodeUnaryOp,
    encodeBinaryOp,
    encodeOperand,
    encodeInstr,
    decodeReg,
    decodeUnaryOp,
    decodeBinaryOp,
    decodeOperand,
    decodeInstr,
  )
where

import A02_Defs
import Data.Bits hiding (Xor, And)
import Data.Word
import GHC.Generics (Generic)
import Generic.Random
import Test.Tasty.QuickCheck as QC hiding
  ( (.&.),
  )

-- | TODO marker.
todo :: t
todo = error "todo"

decodeReg :: Word32 -> Maybe Reg
decodeReg w = if w < 32 then Just (Reg w) else Nothing

decodeUnaryOp :: Word32 -> Maybe UnaryOp
decodeUnaryOp 0 = Just Move
decodeUnaryOp 1 = Just Negate
decodeUnaryOp 2 = Just Complement
decodeUnaryOp 3 = Just Not
decodeUnaryOp w = Nothing

decodeBinaryOp :: Word32 -> Maybe BinaryOp
decodeBinaryOp 0 = Just Add
decodeBinaryOp 1 = Just Sub
decodeBinaryOp 2 = Just Mul
decodeBinaryOp 3 = Just Or
decodeBinaryOp 4 = Just And
decodeBinaryOp 5 = Just Xor
decodeBinaryOp 6 = Just Lt
decodeBinaryOp 7 = Just Gt
decodeBinaryOp 8 = Just Eq
decodeBinaryOp _ = Nothing

get :: Word32 -> Int -> Int -> Word32
get w from size = shiftR w from .&. (shiftL 1 size - 1)

decodeOperand :: Word32 -> Maybe Operand
decodeOperand w
  | get w 0 1 == 0 = OperandReg <$> decodeReg (get w 1 5)
  | otherwise = Just $ OperandVal $ Val $ get w 1 9

decodeInstr :: Word32 -> Maybe Instr
decodeInstr w
  | get w 0 3 == 0 = Unary <$> ((,,) <$> decodeReg (get w 3 5) <*> decodeUnaryOp (get w 8 4) <*> decodeOperand (get w 12 10))
  | get w 0 3 == 1 = Binary <$> ((,,,) <$> decodeReg (get w 3 5) <*> decodeBinaryOp (get w 8 4) <*> decodeOperand (get w 12 10) <*> decodeOperand (get w 22 10))
  | get w 0 3 == 2 = Load <$> ((,) <$> decodeReg (get w 3 5) <*> decodeOperand (get w 12 10))
  | get w 0 3 == 3 = Store <$> ((,) <$> decodeOperand (get w 12 10) <*> decodeOperand (get w 22 10))
  | get w 0 3 == 4 = Cas <$> ((,,) <$> decodeReg (get w 3 5) <*> decodeOperand (get w 12 10) <*> decodeOperand (get w 22 10))
  | get w 0 3 == 5 = Jump <$> decodeOperand (get w 12 10)
  | get w 0 3 == 6 = CondJump <$> ((,,) <$> decodeReg (get w 3 5) <*> decodeOperand (get w 12 10) <*> decodeOperand (get w 22 10))
  | otherwise = Nothing
