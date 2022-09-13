module Tortoise.TortoiseCombinators
       ( andThen
       , loop
       , invisibly
       , retrace
       , overlay
       ) where

import          Tortoise.Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.


andThen :: Instructions -> Instructions -> Instructions
andThen (Move      d  is) i2 = Move d (is `andThen` i2)
andThen (Turn      a  is) i2 = Turn a (is `andThen` i2)
andThen (SetStyle  ls is) i2 = SetStyle ls (is `andThen` i2)
andThen (SetColour c  is) i2 = SetColour c (is `andThen` i2)
andThen (PenDown is     ) i2 = PenDown (is `andThen` i2)
andThen (PenUp   is     ) i2 = PenUp (is `andThen` i2)
andThen Stop              i2 = i2

loop :: Int -> Instructions -> Instructions
loop n i = go n   where
      go n | n <= 0 = Stop
      go n | n > 0  = i `andThen` go (n - 1)


filterDown :: Instructions -> Bool -> (Instructions, Bool)
filterDown (Move d is) z =
       let (new_instr, r) = filterDown is z in (Move d new_instr, r)
filterDown (Turn a is) z =
       let (new_instr, r) = filterDown is z in (Turn a new_instr, r)
filterDown (SetStyle ls is) z =
       let (new_instr, r) = filterDown is z in (SetStyle ls new_instr, r)
filterDown (SetColour c is) z =
       let (new_instr, r) = filterDown is z in (SetColour c new_instr, r)
filterDown (PenDown is) _ = filterDown is True
filterDown (PenUp   is) _ = filterDown is False
filterDown Stop         z = (Stop, z)

retrace' :: Instructions -> Instructions -> TortoiseState -> Instructions
retrace' (Move d is) acc s = retrace' is (Move (-d) acc) s
retrace' (Turn a is) acc s = retrace' is (Turn (-a) acc) s
retrace' (SetStyle ls is) acc s =
       retrace' is (SetStyle (style s) acc) (s { style = ls })
retrace' (SetColour c is) acc s =
       retrace' is (SetColour (colour s) acc) (s { colour = c })
retrace' (PenDown is) acc s
       | penDown s = retrace' is acc (s { penDown = True })
       | otherwise = retrace' is (PenUp acc) (s { penDown = True })
retrace' (PenUp is) acc s
       | penDown s = retrace' is (PenDown acc) (s { penDown = False })
       | otherwise = retrace' is acc (s { penDown = False })
retrace' Stop acc s = acc

retrace is = retrace' is Stop start

invisibly :: Instructions -> Instructions
invisibly i =
       PenUp Stop
              `andThen` xs
              `andThen` (if last then PenDown Stop else PenUp Stop)
       where (xs, last) = filterDown i (True)


overlay :: [Instructions] -> Instructions
overlay []       = Stop
overlay (x : xs) = x `andThen` (invisibly (retrace x)) `andThen` (overlay xs)

