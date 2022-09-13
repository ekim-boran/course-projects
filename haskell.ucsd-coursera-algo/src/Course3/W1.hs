module Course3.W1 where

import Course3.TestGraphs
import Data.Array (accumArray)
import Data.Array qualified as A
import Data.IntSet qualified as S
import Data.Tuple (swap)
import Graph

q1 (graph, start, end) = any (== end) $ reachable graph start

q2 = length . cc

--

testQ1 = q1 <$> [(g1, 1, 4), ((connectedTestGraph 10000 1000), 1, 10000), (g2, 1, 4)]

testQ2 = q2 <$> [g1, connectedTestGraph 1000 100, g2]
