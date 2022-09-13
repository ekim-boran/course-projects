{-# LANGUAGE FlexibleInstances #-}

module A03
  ( interpInstr,
    interpStep,
    runInstr,
    runStep,
  )
where

import A02
import A02_Defs
import A03_Defs
import Control.Monad.State.Lazy
import Data.Bits
import Data.List
import Data.Map as Map
import Data.Maybe
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import Generic.Random
import Test.Tasty.QuickCheck as QC hiding
  ( (.&.),
  )
import Prelude hiding (read)

-- | TODO marker.
todo :: t
todo = error "todo"

-- | Interpret an instruction.
--
-- For the detailed specification, please consult the test cases.
interpInstr :: CoreState m => Instr -> m InstrResult
interpInstr (Binary (reg, op, op1, op2)) =
  interpBinaryOp op <$> interpOperand op1 <*> interpOperand op2 >>= regW reg >> return Normal
interpInstr (Unary (reg, op, op1)) = interpOperand op1 >>= regW reg . interpUnaryOp op >> return Normal
interpInstr (Load (reg, operand)) = do
  op1 <- interpOperand operand
  mem <- memL (Loc op1)
  maybe (ctrlFault InvalidLoad >> return Fault) (\x -> regW reg x >> return Normal) mem
interpInstr (Store (operand1, operand2)) = do
  op1 <- interpOperand operand1
  op2 <- interpOperand operand2
  memS (Loc op1) op2
  return Normal
interpInstr (Cas (reg, operand1, operand2)) = do
  val1 <- regR reg
  op1 <- interpOperand operand1
  op2 <- interpOperand operand2
  r <- memCas (Loc val1) op1 op2
  case r of
    Nothing -> ctrlFault InvalidLoad >> return Fault
    (Just (ok, val)) -> regW reg val >> return Normal
interpInstr (Jump operand1) = interpOperand operand1 >>= (return . JumpTo . Loc)
interpInstr (CondJump (reg, operand1, operand2)) = do
  (Val r) <- regR reg
  op1 <- interpOperand operand1
  op2 <- interpOperand operand2
  if r == 0 then return $ JumpTo (Loc op2) else return $ JumpTo (Loc op1)
interpInstr _ = return Fault

-- | Interpret a step.
--
-- You need to:
-- 1. read the `pcReg` register as `pc`.
-- 2. load memory from the `pc` location. If it's invalid, `ctrlFault InvalidLoad`.
-- 3. decode the load value as instruction. If it's invalid, `ctrlFault InvalidInstr`.
-- 4. interpret the instruction.
-- 5-1. if the result is normal, add 1 to `pcReg`.
-- 5-2. if the result is fault, return ().
-- 5-3. if the result is jumpTo, jump to the specified location.
interpStep :: CoreState m => m ()
interpStep = do
  (Val memLoc) <- regR pcReg
  instr <- memL (Loc (Val memLoc))
  maybe (ctrlFault InvalidLoad) (\(Val v) -> maybe (ctrlFault InvalidInstr) (f memLoc <=< interpInstr) $ decodeInstr v) instr
  where
    f memLoc Normal = regW pcReg (Val $ memLoc + 1)
    f _ (JumpTo (Loc loc)) = regW pcReg loc
    f _ _ = return ()

-- | Execute an instruction.
runInstr :: RegFile -> Mem -> Instr -> (InstrResult, (RegFile, Mem))
runInstr regFile mem instr = runState (interpInstr instr) (regFile, mem)

-- | Execute a step.
runStep :: RegFile -> Mem -> (RegFile, Mem)
runStep regFile mem = runState interpStep (regFile, mem) |> snd
