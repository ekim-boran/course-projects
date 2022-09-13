module Course2.W3 where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Char (ord)
import Data.List (find, foldl', nub)
import Data.List.Extra
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import SAT.MiniSat (Formula (Not))
import Util (readInt, run')

q1Runner = run' "./data/Course2/w3-1" q1parser q1 print'
  where
    print' = T.unpack . T.strip . T.unlines . filter (not . T.null)

q1parser handle = do
  n <- T.hGetLine handle
  xs <- replicateM (readInt n) $ do
    xs <- (T.words) <$> T.hGetLine handle
    return xs
  return xs

q1 xs = runST $ do
  vec <- V.thaw $ V.replicate 10000000 Nothing
  traverse (go vec) xs
  where
    go vec ("add" : a : b : _) = MV.write vec (readInt a) (Just b) >> return ""
    go vec ("del" : a : _) = MV.write vec (readInt a) Nothing >> return ""
    go vec ("find" : a : _) = fmap (maybe "not found" id) $ MV.read vec (readInt a)

actions = last $ q1 (xs ++ zs ++ ys)
  where
    xs = fmap (\a -> [("add"), (T.pack $ show a), (T.pack $ show a)]) [1 .. 50000]
    zs = fmap (\a -> [("del"), (T.pack $ show a)]) [1 .. 49999]
    ys = fmap (\a -> [("find"), (T.pack $ show a)]) [1 .. 50000]

-- >>>   actions
-- "50000"

-- q2

q2Runner = run' "./data/Course2/w3-2" q2parser q2 print'
  where
    print' = trim . T.unpack . T.unlines . fmap T.strip . mapMaybe id

q2parser handle = do
  buckets <- readInt <$> T.hGetLine handle
  n <- T.hGetLine handle

  xs <- replicateM (readInt n) $ do
    xs <- (T.words) <$> T.hGetLine handle
    return xs
  return (buckets, xs)

q2 (m, xs) = runST $ do
  vec <- V.thaw $ V.replicate m []
  traverse (go vec) $ xs
  where
    go vec ("add" : a : _) = MV.modify vec (\xs -> if any (== a) xs then xs else a : xs) (hash a m) >> return Nothing
    go vec ("del" : a : _) = MV.modify vec (filter (/= a)) (hash a m) >> return Nothing
    go vec ("find" : a : _) = fmap (Just . maybe "no" (const "yes") . (find (== a))) $ MV.read vec (hash a m)
    go vec ("check" : i : _) = (Just . T.unwords) <$> MV.read vec (readInt i)

hash str m = foldl go 0 (reverse $ T.unpack $ T.strip $ str) `mod` m
  where
    go ans c = (ans * 263 + ord c) `mod` 1000000007

-- q3

q3 src pattern = findIndices id $ unfoldr go (r, l, hash l)
  where
    (l, r) = splitAt (length pattern) (src ++ "ü")

    go ((a : rest), xx@(x : cur), curHash) =
      Just (r, (rest, (cur ++ [a]), curHash - ord x + ord a))
      where
        r = patternHash == curHash && xx == pattern
    go _ = Nothing

    patternHash = hash pattern
    hash = sum . fmap ord

testStr = q3 st "bbasdasdwqewqeasd"
  where
    st = ((replicate 200000 'a') ++ "bbasdasdwqewqeasd" ++ (replicate 200000 'a'))

-- >>> testStr
-- [2000000]

-- >>> q3 "aaaaa" "aaa"
-- [0,1,2]

-- >>> q3 "ZtonpqnFzlpvUKZrBbRlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXxtHmTxoLuMbRYsvSpxhtrlvABBlFYmndFzHypOmJyFxjHEPlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXbDiEAvtPlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXRRNoBCUMQVOlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXRLKlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXAYPDKWtVpShhclNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXOJlUlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXglmlNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUXuaOibGlVrwghvNTgLfltIbEdBlgjelFjQkBeFrdEV" "lNoYhXmlwOscxnkTWjsyNJNhgvzMFbxFnbiWuBAGjZQlCRQHjTUX"
-- [19,118,178,241,296,361,417,472]

-- q4 --

makeTable x m = V.fromList . scanl (\h i -> (h * x + ord i) `mod` m) 0

lookup' x m table start len = (table V.! (start + len) - a) `mod` m
  where
    a = ((pow x len) * table V.! start) `mod` m
    -- standard library ?
    pow a 1 = a
    pow a n = (a * pow (a) (n - 1)) `mod` m

q4 str xs = go <$> xs
  where
    m = 1000000007
    n = 1000000009
    x = 263
    f1 = lookup' x m $ makeTable x m str
    f2 = lookup' x n $ makeTable x n str
    go (a, b, len) = (f1 a len, f1 b len) -- && f2 a len == f2 b len

test1 = q4 st (replicate 1000 (301000, 304000, 1000))
  where
    st = replicate 200000 'a' ++ replicate 200000 'b'

-- >>> test1
