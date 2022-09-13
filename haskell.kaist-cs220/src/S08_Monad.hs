module S08_Monad
  () where

import           Data.Map                      as Map
import           Prelude                 hiding ( read )

data State s o = State (s -> (o, s))

read :: State s s
read = State (\st -> (st, st))

write :: s -> State s ()
write st = State (\_ -> ((), st))

-- Superclass of Monad, easily defined
instance Functor (State s) where
  fmap f ma = do
    a <- ma
    return (f a)

-- Superclass of Monad, easily defined
instance Applicative (State s) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Monad (State s) where
  ma >>= f = State
    (\s0 ->
      let (State mai) = ma
      in  let (b, s1) = (mai s0) in let State mbi = (f b) in mbi s1
    )

  return a = State (\s -> (a, s))

runState :: s -> State s o -> o
runState s (State f) = fst (f s)

counter :: Integer -> State Integer Integer
counter inc = do
  inner <- read
  write (inner + inc)
  return inner

main :: State Integer Integer
main = do
  a <- counter 42
  b <- counter 37
  return (a + b)

_ = runState 0 main -- 42

-- memory

type Loc = Integer
type Val = Integer

data Mem = Mem (Map Loc Val)

type MemT o = State Mem o

load :: Loc -> MemT (Maybe Val)
load loc = do
  (Mem memi) <- read
  return (Map.lookup loc memi)

store :: Loc -> Val -> MemT ()
store loc val = do
  (Mem memi) <- read
  let memiNew = Map.insert loc val memi
  write (Mem memiNew)
  return ()

-- load :: Loc -> MemT (Maybe Val)
-- load loc = State (\mem ->
--   let (Mem memi) = mem in
--   (Map.lookup loc memi, mem))

-- store :: Loc -> Val -> MemT ()
-- store loc val = State (\mem ->
--   let (Mem memi) = mem in
--   ((), Mem (Map.insert loc val memi)))

-- If the location's value is the old value then replace it with the new value, returning whether it was successful and the location's original value.
-- You may think it can be implemented with load and store, but it isn't if we will consider multi-core CPUs. Stay tuned!

cas :: Loc -> Val -> Val -> MemT (Maybe (Bool, Val))
cas loc val1 val2 = do
  val <- load loc
  case val of
    Nothing   -> return Nothing
    Just vali -> if vali == val1
      then do
        store loc val2
        return (Just (True, vali))
      else return (Just (False, vali))

-- cas :: Loc -> Val -> Val -> MemT (Maybe (Bool, Val))
-- cas loc val1 val2 = State (\mem ->
--   let (Mem memi) = mem in
--   let val = Map.lookup loc memi in
--   case val of
--     Nothing -> (Nothing, mem)
--     Just vali ->
--       if vali == val1
--       then (Just (True, vali), Mem (Map.insert loc val2 memi))
--       else (Just (False, vali), mem))

runMem :: MemT o -> o
runMem e = runState (Mem empty) e

simple :: MemT (Maybe Integer, Maybe Integer)
simple = do
  store 1 42
  store 2 37
  cas 1 37 666
  cas 2 37 666
  v1 <- load 1
  v2 <- load 2
  return (v1, v2)

_ = runMem simple -- (Just 42, Just 666)
