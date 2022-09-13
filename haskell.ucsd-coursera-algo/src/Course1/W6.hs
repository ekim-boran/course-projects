module Course1.W6 where

import Control.Applicative
import Control.Monad
import Data.Array qualified as A
import Data.Bifunctor
import Data.List.Extra (maximumOn, minimumOn)
import Data.Tuple.Extra

q1 coins n = q1' (A.listArray (0, length coins - 1) coins) n
  where
    q1' coins n = arr A.! (n, 0)
      where
        arr = A.listArray ((0, 0), (n, length coins)) (fmap go $ (,) <$> [0 .. n] <*> [0 .. (length coins)])
        go (!n, !index) | n == 0 || index == length coins = (0, [])
        go (!n, !index) =
          let cur = coins A.! index
              x = arr A.! (n, index + 1)
              y = bimap (cur +) (cur :) $ arr A.! (n - cur, index + 1)
           in if n < cur then x else maximumOn fst [x, y]

q2 xs | sum xs `mod` 3 /= 0 = Nothing
q2 xs = q2' ((A.listArray (0, length xs - 1) xs)) (sum xs `div` 3)
  where
    q2' gifts n = arr A.! (0, 0, 0, 0)
      where
        arr = A.listArray ((0, 0, 0, 0), (n, n, n, (length gifts))) $ fmap go $ (,,,) <$> [0 .. n] <*> [0 .. n] <*> [0 .. n] <*> [0 .. (length gifts)]
        go x | x == (n, n, n, length gifts) = Just ([], [], [])
        go (a, b, c, d) = asum [(f (cur :)) <$> (join $ (arr `index'` index)) | (f, index) <- zip [first3, second3, third3] indexes]
          where
            cur = gifts A.! d
            indexes = [(a + cur, b, c, d + 1), (a, b + cur, c, d + 1), (a, b, c + cur, d + 1)]

index' arr index = if A.inRange (A.bounds arr) index then Just $ (arr A.! index) else Nothing

q3 :: String -> (Int, Int)
q3 xs = q3' ((A.listArray (0, length xs - 1) xs)) ((length xs) `div` 2)
  where
    q3' ops len = arr A.! (0, len)
      where
        get i = read $ pure $ ops A.! (2 * i)
        getOp i = ops A.! (2 * i + 1)
        arr = A.listArray ((0, 0), (len, len)) (fmap go $ (,) <$> [0 .. len] <*> [0 .. (len)])
        go (a, b) | a == b = (get a, get a)
        go (a, b) | b == a + 1 = (r, r)
          where
            r = execute (getOp a) (get a) (get b)
        go (a, b) = g $ concat [f (getOp n) (arr A.! (a, n)) (arr A.! (n + 1, b)) | n <- [a .. (b - 1)]]
        f o (min1, max1) (min2, max2) = uncurry (execute o) <$> [(min1, min2), (min1, max2), (max1, min2), (max1, max2)]
        g xs = (minimum xs, maximum xs)
        execute '-' = (-)
        execute '+' = (+)
        execute '*' = (*)

testQ1 = q1 [1 .. 300] 10000

testQ2 = q2 $ [1 .. 30]

testQ3 = q3 "5-8+7*4-8+9*5-8+7*4-8+9*5-8+7*4-8+9"

-- >>> testQ1
-- >>> testQ2
-- >>> testQ3
-- (10000,[260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,287,288,289,290,291,292,293,294,295,296])
-- Just ([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19],[17,18,20,21,22,27,30],[23,24,25,26,28,29])
-- (-6877510,12990920)
