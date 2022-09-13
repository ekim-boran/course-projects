module Queue.Bankers where

import Data.Maybe (fromMaybe)
import Data.Type.Bool (Not)
import Queue.Class

data BankersQueue a = BQ !Int [a] !Int [a]

check n xs m ys = if m <= n then BQ n xs m ys else BQ (n + m) (xs ++ reverse ys) 0 []

instance Queue BankersQueue where
  empty = BQ 0 [] 0 []
  isEmpty (BQ n _ _ _) = n == 0
  peekFront (BQ n (x : xs) m r) = Just x
  peekFront _ = Nothing
  pushBack a (BQ n xs m ys) = check n xs (m + 1) (a : ys)
  popFront (BQ n (x : xs) m ys) = Just $ check (n - 1) xs m ys
  popFront _ = Nothing

data BankersDequeue a = BD !Int [a] !Int [a] deriving (Show)

checkDequeue !n xs !m ys
  | n > 4 * m + 1 =
      let xs' = take size1 xs
          rear' = ys ++ reverse (drop size1 xs)
       in BD size1 xs' size2 rear'
  | m > 4 * n + 1 =
      let xs' = xs ++ reverse (drop size1 ys)
          rear' = take size1 ys
       in BD size2 xs' size1 rear'
  | otherwise = BD n xs m ys
  where
    size1 = (n + m) `div` 2
    size2 = (n + m) - size1

instance Queue BankersDequeue where
  empty = BD 0 [] 0 []
  isEmpty (BD n _ _ _) = n == 0
  pushBack !a (BD n xs m ys) = checkDequeue n xs (m + 1) (a : ys)

  peekFront (BD _ (x : xs) _ _) = Just x
  peekFront (BD _ [] _ [x]) = Just x
  peekFront _ = Nothing

  popFront (BD n (x : xs) m ys) = Just $ checkDequeue (n - 1) xs m ys
  popFront (BD n _ m [y]) = Just $ empty
  popFront _ = Nothing

dropWhileFront f bs = case peekFront bs of
  Nothing -> bs
  (Just x) | f x -> dropWhileFront f (fromMaybe undefined $ popFront (bs))
  _ -> bs

lenBD (BD a _ b _) = a + b

instance Dequeue BankersDequeue where
  pushFront !x (BD n xs m ys) = checkDequeue (n + 1) (x : xs) m ys

  peekBack (BD _ _ _ (x : xs)) = Just x
  peekBack (BD _ [x] _ _) = Just x
  peekBack _ = Nothing

  popBack (BD n xs m (y : ys)) = Just $ checkDequeue n xs (m - 1) ys
  popBack (BD n [x] m _) = Just $ empty
  popBack _ = Nothing

test1 :: BankersDequeue Int
test1 = pushFront 3 $ pushFront 2 $ pushFront 1 empty

test2 = peekBack test1

-- >>> test1
-- >>> test2
-- BD 2 [3,2] 1 [1]
-- Just 1
