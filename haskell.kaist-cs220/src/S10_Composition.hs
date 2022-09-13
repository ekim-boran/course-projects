{-# LANGUAGE TupleSections #-}

module S10_Composition
  ( interpSt1
  , decomposeSt
  , swapSt
  ) where

import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.List

interpSt1 :: State (a, b) r -> a -> State b (r, a)
interpSt1 act a =
  state $ \b -> let (r, (a2, b2)) = runState act (a, b) in ((r, a2), b2)

decomposeSt :: State (a, b) r -> StateT a (State b) r
decomposeSt act = StateT $ interpSt1 act

swapSt :: StateT a (State b) r -> StateT b (State a) r
swapSt act = StateT $ \b -> state
  $ \a -> let ((r, a'), b') = runState (runStateT act a) b in ((r, b'), a')

listMaybeToMaybeList :: ListT Maybe a -> MaybeT [] a
listMaybeToMaybeList act = MaybeT $
  case runListT act of
    Nothing -> [Nothing]
    Just l -> fmap Just l

maybeStateToStateMaybe :: MaybeT (State s) a -> StateT s Maybe a
maybeStateToStateMaybe act = StateT $ \s -> do
  let sact = runMaybeT act
  let (a, s2) = runState sact s
  a <- a
  return (a, s2)

listStateToStateList :: ListT (State s) a -> StateT s [] a
listStateToStateList act = StateT $ \s ->
  let lact = runListT act in
  let (al, s2) = runState lact s in
  fmap (,s2) al
