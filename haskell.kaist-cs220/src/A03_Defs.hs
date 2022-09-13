{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module A03_Defs
  ( CoreState (..),
    Event (..),
    RegFile (..),
    Mem (..),
    InstrResult (..),
    Fault (..),
    pcReg,
    ctrlLoc,
    ctrlClear,
    ctrlHalt,
    ctrlFault,
    encodeFault,
    decodeFault,
    interpOperand,
    interpUnaryOp,
    interpBinaryOp,
    (|>),
  )
where

import A02
import A02_Defs
import Control.Monad.State.Lazy
import Data.Bits hiding (And, Xor)
import Data.Map as Map
import Data.Maybe
import Data.Word
import GHC.Generics (Generic)
import Generic.Random
import Test.Tasty.QuickCheck as QC hiding
  ( (.&.),
  )
import Prelude hiding (read)

-- | TODO marker.
todo :: t
todo = error "todo"

-- | function application.
(|>) :: v1 -> (v1 -> v2) -> v2
x |> f = f x

class Monad m => CoreState m where
  regR :: Reg -> m Val
  regW :: Reg -> Val -> m ()
  memL :: Loc -> m (Maybe Val)
  memS :: Loc -> Val -> m ()
  memCas :: Loc -> Val -> Val -> m (Maybe (Bool, Val))

newtype RegFile = RegFile (Map Reg Val) deriving (Eq, Ord, Show, Generic)

newtype Mem = Mem (Map Loc Val) deriving (Eq, Ord, Show, Generic)

instance Arbitrary RegFile where
  arbitrary = genericArbitrary uniform

instance Arbitrary Mem where
  arbitrary = genericArbitrary uniform

instance CoreState (State (RegFile, Mem)) where
  regR reg = do
    (RegFile regFile, _) <- get
    regFile |> Map.lookup reg |> fromMaybe (Val 0) |> return

  regW reg val = do
    (RegFile regFile, mem) <- get
    regFile |> Map.insert reg val |> RegFile |> (,mem) |> put

  memL loc = do
    (_, Mem mem) <- get
    mem |> Map.lookup loc |> return

  memS loc val = do
    (regFile, Mem mem) <- get
    mem |> Map.insert loc val |> Mem |> (regFile,) |> put

  memCas loc val1 val2 = do
    val <- memL loc
    case val of
      Nothing -> return Nothing
      Just val ->
        if val /= val1
          then return $ Just (False, val)
          else do
            memS loc val2
            return $ Just (True, val)

data Event = RegR Reg Val | RegW Reg Val | MemL Loc (Maybe Val) | MemS Loc Val | MemCas Loc Val Val (Maybe Val) deriving (Eq, Ord, Show)

instance CoreState (State (RegFile, Mem, [Event])) where
  regR reg = do
    (RegFile regFile, mem, events) <- get
    let val = regFile |> Map.lookup reg |> fromMaybe (Val 0)
    put (RegFile regFile, mem, events ++ [RegR reg val])
    return val

  regW reg val = do
    (RegFile regFile, mem, events) <- get
    regFile
      |> Map.insert reg val
      |> RegFile
      |> (,mem,events ++ [RegW reg val])
      |> put

  memL loc = do
    (regFile, Mem mem, events) <- get
    let val = mem |> Map.lookup loc
    put (regFile, Mem mem, events ++ [MemL loc val])
    return val

  memS loc val = do
    (regFile, Mem mem, events) <- get
    mem
      |> Map.insert loc val
      |> Mem
      |> (regFile,,events ++ [MemS loc val])
      |> put

  memCas loc val1 val2 = do
    (regFile, Mem mem, events) <- get
    let val = mem |> Map.lookup loc
    let event = MemCas loc val1 val2 val
    case val of
      Nothing -> do
        put (regFile, Mem mem, events ++ [event])
        return Nothing
      Just val -> do
        let isEqual = val == val1
        let memNew = if isEqual then mem |> Map.insert loc val2 else mem
        put (regFile, Mem memNew, events ++ [event])
        return $ Just (isEqual, val)

-- | The program counter's register id.
pcReg :: Reg
pcReg = Reg 0

-- | The control's location id.
ctrlLoc :: Loc
ctrlLoc = Loc (Val 0xdeadbeef)

ctrlClear :: CoreState m => m ()
ctrlClear = memS ctrlLoc (Val 0x0)

ctrlHalt :: CoreState m => m ()
ctrlHalt = memS ctrlLoc (Val 0x10)

data Fault = InvalidLoad | InvalidInstr | InvalidThreadId deriving (Eq, Ord, Show)

encodeFault :: Fault -> Word32
encodeFault InvalidLoad = 0
encodeFault InvalidInstr = 1
encodeFault InvalidThreadId = 2

decodeFault :: Word32 -> Maybe Fault
decodeFault 0 = Just InvalidLoad
decodeFault 1 = Just InvalidInstr
decodeFault 2 = Just InvalidThreadId
decodeFault _ = Nothing

ctrlFault :: CoreState m => Fault -> m ()
ctrlFault fault = memS ctrlLoc (Val $ 0x20 .|. place (encodeFault fault) 0 4)

interpOperand :: CoreState m => Operand -> m Val
interpOperand operand = case operand of
  OperandReg reg -> regR reg
  OperandVal val -> return val

interpUnaryOp :: UnaryOp -> Val -> Val
interpUnaryOp Move (Val val) = Val val
interpUnaryOp Negate (Val val) = Val $ negate val
interpUnaryOp Complement (Val val) = Val $ complement val
interpUnaryOp Not (Val val) = Val $ if val == 0 then 1 else 0

interpBinaryOp :: BinaryOp -> Val -> Val -> Val
interpBinaryOp Add (Val val1) (Val val2) = Val $ val1 + val2
interpBinaryOp Sub (Val val1) (Val val2) = Val $ val1 - val2
interpBinaryOp Mul (Val val1) (Val val2) = Val $ val1 * val2
interpBinaryOp Or (Val val1) (Val val2) = Val $ val1 .|. val2
interpBinaryOp And (Val val1) (Val val2) = Val $ val1 .&. val2
interpBinaryOp Xor (Val val1) (Val val2) = Val $ val1 `xor` val2
interpBinaryOp Lt (Val val1) (Val val2) = Val $ if val1 < val2 then 1 else 0
interpBinaryOp Gt (Val val1) (Val val2) = Val $ if val1 > val2 then 1 else 0
interpBinaryOp Eq (Val val1) (Val val2) = Val $ if val1 == val2 then 1 else 0

data InstrResult = Normal | Fault | JumpTo Loc deriving (Eq, Ord, Show)
