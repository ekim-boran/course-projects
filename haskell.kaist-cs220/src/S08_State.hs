module S08_State
  () where

import           Prelude                 hiding ( read )

counter1 :: (Integer, Integer) -> (Integer, Integer)
counter1 (inc, inner) =
  let result = inner in let innerNew = inner + inc in (result, innerNew)

type State s o = s -> (o, s)

read :: State s s
read st = (st, st)

write :: s -> State s ()
write st _ = ((), st)

-- doesn't compile!
--
-- counter :: Integer -> State Integer Integer
-- counter inc =
--   let inner = read;
--   let _ = write (inner + inc);
--   inner

and_then :: State s a -> (a -> State s b) -> State s b
and_then ma f s0 =
  let (a, s1) = ma s0 in let mb = f a in let (b, s2) = mb s1 in (b, s2)

counter :: Integer -> State Integer Integer
counter inc =
  read
    `and_then` (\inner ->
                 write (inner + inc) `and_then` (\ _ s -> (inner, s))
               )

-- ideal:
--
-- counter :: Integer -> State Integer Integer
-- counter inc = do
--   inner <- read;
--   write (inner + inc);
--   return inner

run :: State Integer o -> o
run mo = fst (mo 0)

-- ideal:
--
-- main :: State o ()
-- main = do
--   a <- counter 42; -- 0
--   b <- counter 37; -- 42
--
-- let _ = run main -- the whole program execution returning ()
