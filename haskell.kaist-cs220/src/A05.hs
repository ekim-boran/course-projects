module A05
  ( ShrMem (..),
    newShrMem,
    loadShrMem,
    storeShrMem,
    casShrMem,
  )
where

import A02_Defs
import A03_Defs
import A05_Defs
import Control.Concurrent.MVar
import Data.Map as Map

-- | TODO marker.
todo :: t
todo = error "todo"

newShrMem :: IO ShrMem
newShrMem = ShrMem <$> newMVar Map.empty

loadShrMem :: Loc -> ShrMem -> IO (Maybe Val)
loadShrMem loc (ShrMem mem) =
  withMVar mem (traverse readMVar . Map.lookup loc)

storeShrMem :: Loc -> Val -> ShrMem -> IO ()
storeShrMem loc val (ShrMem mem) =
  modifyMVar_ mem $ \map' ->
    case Map.lookup loc map' of
      Nothing -> do
        mvar <- newMVar val
        return $ Map.insert loc mvar map'
      (Just mvar) -> modifyMVar_ mvar (return . const val) >> return map'

casShrMem :: Loc -> Val -> Val -> ShrMem -> IO (Maybe (Bool, Val))
casShrMem loc val1 val2 (ShrMem mem) = withMVar mem $ \map' -> do
  case Map.lookup loc map' of
    Nothing -> return Nothing
    (Just mvar) -> do
      val <- readMVar mvar
      if val /= val1
        then return $ Just (False, val)
        else modifyMVar_ mvar (\_ -> return val2) >> return (Just (True, val))

--  memCas loc val1 val2 = do
--    val <- memL loc
--    case val of
--      Nothing  -> return Nothing
--      Just val -> if val /= val1
--        then return $ Just (False, val)
--        else do
--          memS loc val2
--          return $ Just (True, val)