module Course3.W5 where

import Control.Monad.Extra
import Control.Monad.ST
import Course3.TestGraphs
import Data.Array qualified as A
import Data.Bifunctor (first)
import Data.Foldable
import Data.Foldable.Extra qualified as S
import Data.IntMap qualified as M
import Data.List
import Data.Set qualified as S
import Data.Tuple (swap)
import Data.Tuple.Extra
import Data.Vector.Mutable qualified as V
import DisjointUnion
import Graph
import Heap.BinomialHeap qualified as H

q1 = prim

q2 :: WGraph -> Double
q2 g = runST $ do
  du <- mkDu [0 .. (length g)]
  let edges = sortOn (snd . snd) $ allEdges g
  add edges du
  where
    add xs du = go xs
      where
        go ((s, (e, w)) : xs) = do
          n <- groups du
          if n < 4
            then return w
            else connect s e du >> go xs
        go _ = return $ (-1.0)
    groups (DU vec) = S.length <$> V.foldl' (\set (a, b) -> S.insert a set) S.empty vec

testQ1 = sum $ fst3 <$> q1 wg8

-- >>> testQ1
-- >>> testQ2
-- 7.06449510224598
--
