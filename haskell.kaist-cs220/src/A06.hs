{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module A06 (interpMachine) where

import A02_Defs
import A03
import A03_Defs
import A04_Defs
import A05
import A05_Defs
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State.Lazy
import Data.Map as Map
import Data.Maybe
import Data.Word
import Debug.Trace
import System.IO.Unsafe
import Text.Printf

-- | TODO marker.
todo :: t
todo = error "todo"

forkOSResult :: IO a -> IO (MVar a)
forkOSResult act = do
  var <- newEmptyMVar
  forkOS $ do
    value <- act
    putMVar var value
  return var

instance CoreState (StateT RegFile (StateT ShrMem IO)) where
  regR reg = do
    RegFile regFile <- get
    regFile |> Map.lookup reg |> fromMaybe (Val 0) |> return

  regW reg val = do
    RegFile regFile <- get
    regFile |> Map.insert reg val |> RegFile |> put

  memL loc = lift get >>= (liftIO . loadShrMem loc)
  memS loc val = lift get >>= (liftIO . storeShrMem loc val)
  memCas loc val1 val2 = lift get >>= (liftIO . casShrMem loc val1 val2)

interpMachine :: StateT (Map Word32 RegFile) (StateT ShrMem IO) ()
interpMachine = StateT $ \regFiles -> StateT $ \shrMem -> do
  regFiles <- forM regFiles $ \regFile -> forkOSResult $ loop regFile shrMem
  regFiles <- forM regFiles takeMVar
  return (((), regFiles), shrMem)
  where
    loop :: RegFile -> ShrMem -> IO RegFile
    loop regFile shrMem = do
      val <- loadShrMem (Loc $ Val 0x30) shrMem
      ((_, regFileNew), _) <- runStateT (runStateT (interpStep) regFile) shrMem
      fault <- loadShrMem ctrlLoc shrMem
      if fault == Just (Val 0x10)
        then return regFileNew
        else loop regFileNew shrMem
