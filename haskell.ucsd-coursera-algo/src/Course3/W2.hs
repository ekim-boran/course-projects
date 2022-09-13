module Course3.W2 where

import Graph
import Course3.TestGraphs
import Data.IntSet qualified as S

q1 = hasCycle

q2 = topologicalSort

q3 = length . scc

---

testQ1 = q1 <$> [g1, g4]

testQ2 = q2 <$> [g3, g4]

testQ3 = q3 <$> [g1, g4]

-- >>> testQ2
-- >>> testQ3

-- q1 g = or $ fmap (snd) $ explore pre post (S.empty, False) g (nodes g)
--  where
--    pre key (set, b) = (S.insert key set, b)
--    post key (set, b) = (set, b || S.member key set)
--
---- toposort
--
-- q2 g = reverse $ concatMap (reverse) $ explore pre post [] g (nodes g)
--  where
--    pre key xs = xs
--    post key xs = key : xs
--

-- q3 g = explore pre doNothing [] reversed (q2 g)
--  where
--    pre k xs = k : xs
--    reversed = reverseGraph g
--
