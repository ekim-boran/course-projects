module S02_Collection
  () where

data IntegerList = Nil | Cons Integer IntegerList

v1 :: IntegerList
v1 = Nil                 -- []

v2 :: IntegerList
v2 = Cons 1 Nil          -- [1]

v3 :: IntegerList
v3 = Cons 2 (Cons 1 Nil) -- [2, 1]

v4 :: [Integer]          -- parametric polymorphism (later)
v4 = [2, 1]

v5 :: [[Float]]          -- list of lists of floats
v5 = [[2.0, 1.0], [], [1.0, 2.0]]

v6 :: [Integer]          -- mimicking sets
v6 = [ x | x <- [1 .. (min 100 1000)], 100 `rem` x == 0, 1000 `rem` x == 0 ]


-- Modeling set with list

v7 :: [Integer]
v7 = [1, 1]              -- corresponding to {1}, not {1, 1}

v8 :: [Integer]
v8 = [1, 2]

v9 :: [Integer]
v9 = [2, 1]              -- v6 and v7 differ, while they both represents {1, 2}


-- Modeling relation with list
v10 :: [(Integer, Integer)]
v10 = [ (x, x + 1) | x <- [0 .. 1000] ]
