module Util where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Extra (trim)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read
import RIO.FilePath
import System.Directory.Extra (listDirectory)
import System.IO

run ncases check dir' f = do
  let dir = "./data/" ++ dir'
  xs <- listDirectory dir
  xs' <- concat <$> traverse (check f dir) (addPath dir $ take (ncases * 2) $ sort xs)
  if null xs' then print "All tests past" else putStrLn xs'
  where
    addPath d (a : b : xs) = (d ++ "/" ++ a, d ++ "/" ++ b) : addPath d xs
    addPath d _ = []

check f dir (file, sol) = do
  xs <- trim <$> readFile file
  sol' <- trim <$> readFile sol
  print $ f xs
  print $ sol'

  if f xs /= sol' then return (show file ++ "failed\n") else return ""

checkT f dir (file, sol) = do
  xs <- T.dropAround isSpace <$> T.readFile file
  sol' <- T.dropAround isSpace <$> T.readFile sol
  let sol'' = T.unpack sol'
  if f xs /= sol'' then return (show file ++ " " ++ f xs) else return ""

parse :: T.Text -> Int
parse xs = case signed decimal xs of
  Left _ -> undefined
  (Right (x, xs')) -> fromIntegral x

run' dirPath parser q printer = do
  xs <- listDirectory dirPath
  let names = sort $ nub $ fmap takeBaseName xs
  forM_ (names) $ \name -> do
    print name
    handle <- openFile (dirPath ++ "/" ++ name) ReadMode
    r <- parser handle
    let result = printer $ q r
    sol <- fmap trim $ readFile $ dirPath ++ "/" ++ name ++ ".a"
    let x = zip result sol
    when (result /= sol) $ error $ "error " ++ name ++ " " ++ result
  return True

readInt :: T.Text -> Int
readInt xs = case signed decimal xs of
  Left _ -> undefined
  (Right (x, xs')) -> fromIntegral x

readInts = fmap readInt . T.words
