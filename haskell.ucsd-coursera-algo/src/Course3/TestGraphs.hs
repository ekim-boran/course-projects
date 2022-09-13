module Course3.TestGraphs where

import Graph

g1 = buildUG 4 [(1, 2), (4, 1), (2, 3), (3, 1)]

g2 = buildUG 5 [(5, 2), (4, 2), (3, 4), (1, 4)]

g3 = buildG 4 [(1, 2), (4, 1), (2, 3)]

g4 = buildG 5 [(1, 2), (2, 3), (1, 3), (3, 4), (1, 4), (2, 5), (3, 5)]

connectedTestGraph n m = buildUG n [(a, b) | a <- [1 .. (n - m)], b <- [(a + 1) .. ((a + m))]]

wg1 = buildWGraph 4 [(1, 2, 1), (4, 1, 2), (2, 3, 2), (1, 3, 5)]

wg2 = buildWGraph 5 [(1, 2, 4), (1, 3, 2), (2, 3, 2), (3, 2, 1), (2, 4, 2), (3, 5, 4), (5, 4, 1), (2, 5, 3), (3, 4, 4)]

wg3 = buildWGraph 3 [(1, 2, 7), (1, 3, 5), (2, 3, 2)]

wg4 = buildWGraph 4 [(1, 2, -5), (4, 1, 2), (2, 3, 2), (3, 1, 1)]

wg5 = buildWGraph 6 [(1, 2, 10), (2, 3, 5), (1, 3, 100), (3, 5, 7), (5, 4, 10), (4, 3, -18), (6, 1, -1)]

wg6 = buildWGraph 5 [(1, 2, 1), (4, 1, 2), (2, 3, 2), (3, 1, -5)]

graphFromPoint xs = [(index, index2, distance x y) | (index, x) <- pts, (index2, y) <- pts, index /= index2]
  where
    pts = zip [1 ..] xs
    distance (a, b) (c, d) = sqrt $ ((a - c) ^ 2) + ((b - d) ^ 2)

wg7 = buildWGraph 4 $ graphFromPoint [(0, 0), (0, 1), (1, 0), (1, 1)]

wg8 = buildWGraph 5 $ graphFromPoint [(0, 0), (0, 2), (1, 1), (3, 0), (3, 2)]

wg9 = buildWGraph (length pts) (graphFromPoint pts)
  where
    pts = [(7, 6), (4, 3), (5, 1), (1, 7), (2, 7), (5, 7), (3, 3), (7, 8), (2, 8), (4, 4), (6, 7), (2, 6)]
