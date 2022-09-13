module DisjointUnion where

import Control.Monad.ST
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as V

newtype DU s a = DU {du :: V.MVector s (Int, a)}

runDU :: (forall s. ST s (DU s a)) -> V.Vector (Int, a)
runDU s = runST (s >>= (V.freeze . du))

mkDu :: [Int] -> ST s (DU s (Int))
mkDu xs = DU <$> V.thaw (V.fromList $ zip [0 .. (length xs)] xs)

parent i du@(DU vec) = do
  (i', a) <- V.read vec i
  if i' == i
    then return i
    else do
      p <- parent i' du
      V.write vec i (p, a)
      return p

connect x y du@(DU vec) = do
  py <- parent y du
  px <- parent x du
  (_, sizey) <- V.read vec py
  (_, sizex) <- V.read vec px
  if py == px
    then return sizex
    else do
      V.write vec py (px, 0)
      V.write vec px (px, sizex + sizey)
      return $ sizex + sizey

isConnected x y du@(DU vec) = do
  py <- parent y du
  px <- parent x du
  return $ py == px

-- >>>  test
-- [10,10,10,11]
--
