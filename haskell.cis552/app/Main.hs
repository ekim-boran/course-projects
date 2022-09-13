module Main where

import Data.List
import qualified Data.Set as S
import qualified Data.Vector as V
import Debug.Trace
import HW5.RegExp
import HW6.HW6
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import HW6.Tests

main :: IO ()
main = quickCheck prop_roundtrip --print $ det3 $ V.fromList $ [V.fromList [1 .. 11] | x <- [1 .. 11]]

--allEqual :: (Num a, Eq a) => V.Vector (V.Vector a) -> Bool
--allEqual = V.foldl (\acc x -> acc && alleq x) True
--  where
--    alleq xs = V.null xs || V.all (== (V.head xs)) (V.tail xs)
--
--sub2 i xs = fmap snd $ V.filter (\(j, xs) -> i /= j) $ V.zip (V.fromList [1 .. (V.length $ V.head xs)]) (fmap (V.tail) xs)
--
--det3 :: (Num a, Eq a) => V.Vector (V.Vector a) -> a
--det3 x
--  | s == 0 = 0
--  | s == 1 = V.head $ V.head x
--  | s == 2 = (V.head (V.head x) * ((x V.! 1) V.! 1)) - ((V.head x V.! 1) * V.head (x V.! 1))
--  | allEqual x = 0
--  | otherwise = sum [((-1) ^ (i + 1)) * V.head (x V.! (i - 1)) * det3 (sub2 i x) | i <- [1 .. s]]
--  where
--    s = length (x)
--
---- >>> :set +s
--
--a = det3 $ V.fromList [V.fromList [6, 1, 1], V.fromList [4, -2, 5], V.fromList [2, 8, 7]]
--
---- >>> a
---- -306
----
--
--data Matrix = Matrix
--  { items :: V.Vector (V.Vector Int),
--    cols :: S.Set Int,
--    startRow :: Int,
--    n :: Int
--  }
--
--det4 :: Matrix -> Int
--det4 (Matrix {..})
--  | n - startRow == 2 =
--  | otherwise  =
--