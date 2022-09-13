{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Test.Tasty
import Test.Tasty.JsonReporter
import Control.Exception
import System.Exit
import qualified Data.List        as L

main :: IO ()
main = do
  sc <- initScore
  tests <- gatherTests okFile "tests/" sc
  runTests tests `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

runTests :: TestTree -> IO ()
runTests = defaultMainWithIngredients (consoleAndJsonReporter : defaultIngredients) 

okFile :: FilePath -> Bool
okFile f = L.isSuffixOf ".json" f 
        -- && not (takeFileName f `elem` ["yourTests.json"])


