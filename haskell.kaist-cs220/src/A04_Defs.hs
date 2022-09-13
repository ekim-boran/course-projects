{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module A04_Defs
  ( CoreState (..),
    runner,
    run,
  )
where

import A02_Defs
import A03
import A03_Defs
import Control.Monad.State.Lazy
import Data.Bits
import Data.Map as Map
import Data.Maybe
import Data.Word
import GHC.Generics (Generic)
import Generic.Random
import S10_Composition
import Test.Tasty.QuickCheck as QC hiding
  ( (.&.),
  )
import Prelude hiding (read)

run = interpSt1 interpStep :: RegFile -> State Mem ((), RegFile)

runner = decomposeSt interpStep :: StateT RegFile (State Mem) ()

_ = swapSt $ decomposeSt interpStep :: StateT Mem (State RegFile) ()

instance CoreState (StateT RegFile (State Mem)) where
  regR reg = do
    RegFile regFile <- get
    regFile |> Map.lookup reg |> fromMaybe (Val 0) |> return

  regW reg val = do
    RegFile regFile <- get
    regFile |> Map.insert reg val |> RegFile |> put

  memL loc = do
    Mem mem <- lift get
    mem |> Map.lookup loc |> return

  memS loc val = do
    Mem mem <- lift get
    mem |> Map.insert loc val |> Mem |> put |> lift

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
