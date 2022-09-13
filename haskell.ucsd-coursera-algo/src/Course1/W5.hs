module Course1.W5 where

import Data.Array qualified as A
import Data.ByteString qualified as BS
import Data.List.Extra
import Data.String

q1 coins n = arr A.! n
  where
    arr = A.listArray (0, n) (fmap go [0 .. n])
    go 0 = (0, [])
    go n = minimumOn fst [(x + 1, c : xs) | c <- coins, n - c >= 0, let (x, xs) = arr A.! (n - c)]

---

data Op = Add Int | Mul Int deriving (Show, Eq, Ord)

execute (Mul i) n =
  let (d, m) = n `divMod` i
   in if m == 0 then [d] else []
execute (Add i) n = if n - 1 >= 0 then [n - i] else []

q2 ops n = reverse $ snd $ arr A.! n
  where
    arr = A.listArray (0, n) (fmap go [0 .. n])
    go 1 = (1, [1])
    go n = minimumOn fst [(x + 1, n : xs) | op <- ops, r <- execute op n, let (x, xs) = arr A.! r]

---

q3 w1 w2 = arr A.! (0, 0)
  where
    (n1, n2) = (BS.length w1, BS.length w2)
    arr = A.listArray ((0, 0), (n1, n2)) (fmap go $ (,) <$> [0 .. n1] <*> [0 .. n2])
    go (x, y) | x == n1 || y == n2 = (n1 - x) + (n2 - y)
    go (x, y) | w1 `BS.index` x == w2 `BS.index` y = arr A.! (x + 1, y + 1)
    go (x, y) = 1 + (foldl1 min [arr A.! (x + 1, y), arr A.! (x, y + 1), arr A.! (x + 1, y + 1)])

---

q4 w1 w2 = arr A.! (0, 0)
  where
    (n1, n2) = (BS.length w1, BS.length w2)
    arr = A.listArray ((0, 0), (n1, n2)) (fmap go $ (,) <$> [0 .. n1] <*> [0 .. n2])
    go (x, y) | x == n1 || y == n2 = 0
    go (x, y) | w1 `BS.index` x == w2 `BS.index` y = 1 + arr A.! (x + 1, y + 1)
    go (x, y) = foldl1 max [arr A.! (x + 1, y), arr A.! (x, y + 1)]

--
q5 w1 w2 w3 = arr A.! (0, 0, 0)
  where
    (n1, n2, n3) = (BS.length w1, BS.length w2, BS.length w3)
    arr = A.listArray ((0, 0, 0), (n1, n2, n3)) (fmap go $ (,,) <$> [0 .. n1] <*> [0 .. n2] <*> [0 .. n3])
    go (x, y, z) | x == n1 || y == n2 || n3 == z = 0
    go (x, y, z)
      | w1 `BS.index` x == w2 `BS.index` y && w2 `BS.index` y == w3 `BS.index` z =
          1 + arr A.! (x + 1, y + 1, z + 1)
    go (x, y, z) = foldl1 max [arr A.! (x + 1, y, z), arr A.! (x, y + 1, z), arr A.! (x, y, z + 1)]

-- tests

testQ1 = q1 [1, 3, 4] 999

testQ2 = q2 [Add 1, Mul 2, Mul 3] 1000000

makeBS n str = fromString $ concat $ replicate n str

q3Test = q3 (makeBS 24 "aaca") (makeBS 25 "bbcb")

q4Test = q4 (makeBS 25 "aaca") (makeBS 25 "bbcb")

q5Test = q5 (makeBS 100 "a") (makeBS 100 "b") (makeBS 100 "c")

-- >>> q5Test
-- 0
