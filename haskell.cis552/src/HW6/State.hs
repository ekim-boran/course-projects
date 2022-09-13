--Since state is a handy thing to have, the standard library includes a
--[module][1] `Control.Monad.State` that defines a parameterized version
--of the state-transformer monad.  This file is a simplified version of
--that library.
--
--We will only allow clients to use the functions declared below.

module HW6.State (State, get, put, runState, evalState, execState, gets, modify) where

import Control.Monad

--The type definition for a generic state transformer is very simple:

newtype State s a = S (s -> (a, s))

--It is a parameterized state-transformer monad where the state is
--denoted by type `s` and the return value of the transformer is the
--type `a`. We make the above a monad by declaring it to be an instance
--of the `monad` typeclass

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = S (\s -> (x, s))
  st >>= f =
    S
      ( \s ->
          let (x, s') = runState st s
           in runState (f x) s'
      )

--where the function `runState` (called `apply` in the lecture) is just

runState :: State s a -> s -> (a, s)
runState (S f) = f

--There are also two other was of evaluating the state monad. The first
--only returns the final result,

evalState :: State s a -> s -> a
evalState s = fst . runState s

--and the second only returns the final state.

execState :: State s a -> s -> s
execState s = snd . runState s

gets f = S (\s -> (f s, s))

--Accessing and Modifying State
-------------------------------
--
--Since our notion of state is generic, it is useful to write
--a `get` and `put` function with which one can *access* and

-- * modify* the state. We can easily `get` the *current* state

--via

get :: State s s
get = S (\s -> (s, s))

--That is, `get` denotes an action that leaves the state unchanged, but
--returns the state itself as a value. Note that although get *does not*
--have a function type (unless you peek under the covers of State), we
--consider it a monadic "action".
--
--Dually, to *modify* the state to some new value `s'` we can write
--the function

put :: s -> State s ()
put s = S (const ((), s))

modify f = S (\s -> ((), f s))

--which denotes an action that ignores (ie blows away) the old state and
--replaces it with `s`. Note that the `put s` is an action that itself
--yields nothing (that is, merely the unit value.)
--
--
--
--
--[1]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2
