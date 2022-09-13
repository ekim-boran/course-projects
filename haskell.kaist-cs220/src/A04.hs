{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module A04
  ( interpThread,
  )
where

import A02
import A02_Defs
import A03
import A03_Defs
import A04_Defs (CoreState (..), runner)
import Control.Monad.State.Lazy
import Data.Bits
import Data.Map as Map
import Data.Maybe
import Data.Word
import S10_Composition
import Prelude hiding (read)

-- | TODO marker.
todo :: t
todo = error "todo"

-- | Interpret a thread.
--
-- `Map Word32 RegFile` maps thread id to its state, i.e., register file.
--
-- `interpThread` is given a thread id, and you need to access the thread states and memory to execute the thread of the given id.
-- In doing so, the corresponding thread state and memory should be read and written.
--
-- If the given thread id doesn't have a thread, invoke `InvalidThreadId` fault (i.e., writing `0x22` to `0xdeadbeef`).
interpThread :: Word32 -> StateT (Map Word32 RegFile) (State Mem) ()
interpThread tid = do
  mregfile <- gets (Map.lookup tid)
  StateT $ \s -> case mregfile of
    Nothing ->
      execStateT (ctrlFault InvalidThreadId) (RegFile Map.empty) >> return ((), s)
    (Just regfile) -> do
      r <- execStateT runner regfile
      return ((), Map.insert tid r s)
