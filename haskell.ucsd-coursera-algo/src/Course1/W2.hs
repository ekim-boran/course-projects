module Course1.W2 where

import Data.List
import Data.Tuple.Extra
import Prelude hiding (gcd, lcm)

--- q1
fib1 n
  | n <= 1 = 1
  | otherwise = fib1 (n - 1) + fib1 (n - 2)

--- q2
fibs base = xs
  where
    xs = 0 : scanl (\a b -> (a + b) `mod` base) 1 xs

lastDigit n = fibs 10 !! n

q2 = lastDigit

q2Test = [0, 1, 5, 9, 5] == (lastDigit <$> [0, 1, 10, 331, 327305])

-- q3
gcd a b
  | b == 0 = a
  | otherwise = gcd b (a `mod` b)

q3 = gcd

-- q4
lcm :: Int -> Int -> Int
lcm a b = a * b `div` gcd a b

q4 = lcm

-- q5
pisanoPeriod base = 0 : go (tail $ fibs base)
  where
    go (0 : 1 : rest) = []
    go (x : xs) = x : go xs

index xs n = xs !! (n `mod` length xs)

lastNumber base n = pisanoPeriod base `index` n

q5 = lastNumber

q5Test = (uncurry lastNumber <$> [(239, 2816213588), (1000, 239)]) == [151, 161]

-- >>> q5Test

-- q6

sumLast n = (a * sum xs + sum (take (n' + 1) xs))
  where
    xs = pisanoPeriod 10
    (a, n') = n `divMod` (length xs)

q6 n = sumLast n `mod` 10

q6Test = (q6 <$> [3, 100]) == [4, 5]

-- >>> q6Test
-- True
--

q7 start end = abs ((sumLast end - sumLast (start - 1)) `mod` 10)

q7Test = (uncurry q7 <$> [(10, 200), (3, 7)]) == [2, 1]

-- >>> [q7 10 200, q7 3 7]
-- [2,1]
--

-- explained in pdf 
q8 n = (lastDigit 10 n * lastDigit 10 (n + 1)) `mod` 10

-- >>> q8 <$> [7, 1234567890, 73]
-- [3,0,1]
--
