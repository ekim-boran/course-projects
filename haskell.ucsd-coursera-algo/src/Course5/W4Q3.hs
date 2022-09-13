module Course5.W4Q3 where

import Control.Monad (replicateM)
import Data.IntMap qualified as M
import Data.IntSet qualified as S
import Data.List (foldl', permutations, sortOn)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

fullyConnected n = [(a, b, 7 * a + 3 * b) | a <- [1 .. n], b <- [(a + 1) .. n], a < b]

q3Tests =
  [ (1, 2, 20),
    (1, 3, 42),
    (1, 4, 35),
    (2, 3, 30),
    (2, 4, 34),
    (3, 4, 12),
    (4, 5, 10),
    (3, 6, 12),
    (5, 6, 20),
    (1, 6, 12)
  ]

q3Tests2 =
  [ (1, 2, 1),
    (2, 3, 4),
    (3, 4, 5),
    (4, 2, 1)
  ]

-- branch and bound

printResult (total, path) =
  if total == maxBound
    then putStrLn "-1"
    else do
      print total
      putStrLn $ unwords $ fmap show path

main = do
  l <- getLine
  let (nnodes : nlines : _) = read <$> words l
  xs <- replicateM nlines $ do
    (s : t : w : _) <- (fmap read . words) <$> getLine
    return (s, t, w)
  printResult $ q3 nnodes xs

trace' s a = a
 
naive edges = minimum $ mapMaybe (score 1) ps
  where
    ps = permutations (filter (/= 1) $ M.keys g)
    g = M.map (M.fromList) $ M.fromListWith (++) $ concat [[(s, [(e, w)]), (e, [(s, w)])] | (s, e, w) <- edges]
    score :: Int -> [Int] -> Maybe Int
    score cur (x : xs) = case M.lookup x g >>= M.lookup cur of
      Nothing -> Nothing
      (Just w) -> (w +) <$> (score x xs)
    score cur [] = M.lookup 1 g >>= M.lookup cur

q3 :: Int -> [(Int, Int, Int)] -> (Int, [Int])
q3 n edges = go (maxBound, []) 0 (S.empty, []) 1
  where
    g = M.fromListWith (++) $ concat [[(s, [(e, w)]), (e, [(s, w)])] | (s, e, w) <- edges]
    go (!b, p) !curweight _ _ | curweight >= b = (b, p)
    go bound curweight (s, path) node | node == 1 && S.size s == n = trace' (show (curweight, path)) (curweight, path)
    go bound curweight (s, path) node | node == 1 && curweight /= 0 = bound
    go bound curweight (s, path) node = foldl' (\m (t, w) -> go m (curweight + w) (S.insert t s, t : path) t) bound xs
      where
        xs = filter (\(t, w) -> t `S.notMember` s) $ g M.! node

-- >>> q3 5 (fullyConnected 5)
-- (126,[1,4,2,3,5])
--
-- >>> naive   (fullyConnected 5)
-- 126
--
test = print $ q3 12 (fullyConnected 12)
