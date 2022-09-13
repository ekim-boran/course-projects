module Course3.W3 where

import Control.Applicative
import Graph
import Course3.TestGraphs
import Data.Array qualified as A
import Data.Foldable (find)
import Data.IntMap qualified as M

-- bfs
-- shortest path
-- bipartite

q1 (g, s, e) = fmap snd $ find ((== e) . fst) $ head $ levels $ bfs g [s]

q2 = bipartite

testQ1 = q1 <$> [(g1, 1, 3), (g2, 3, 4)]

testQ2 = q2 <$> [g1, g2]

-- >>> testQ1
-- [Just 2,Just 1]

-- q1 g start end = bfs pre Nothing g [start]
--  where
--    pre (n, c) x
--      | end == n = x <|> Just c
--      | otherwise = x
--
-- q2 g = and $ snd <$> bfs pre (M.empty, True) g (nodes g)
--  where
--    pre (n, c) (map, b) = (M.insert n c map, b && and [maybe True (f c) $ M.lookup e map | e <- edges g n])
--    f n
--      | odd n = even
--      | otherwise = odd
--
