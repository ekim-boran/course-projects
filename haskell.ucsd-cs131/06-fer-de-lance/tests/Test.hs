{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Test.Tasty
import Text.Printf
import Control.Exception
import System.Exit
import Language.FDL.Types      hiding (Result)
import Paths_fdl

main :: IO ()
main = do
  sc <- initScore

  testsFile        <- getDataFileName "tests/adder.json"
  boaTestsFile     <- getDataFileName "tests/boa.json"
  cobraTestsFile   <- getDataFileName "tests/cobra.json"
  anfTestsFile     <- getDataFileName "tests/anf.json"
  dynamicTestsFile <- getDataFileName "tests/dynamic.json"
  staticTestsFile <- getDataFileName "tests/static.json"
  diamondTestsFile <- getDataFileName "tests/diamondback.json"
  eggTestsFile     <- getDataFileName "tests/egg.json"
  fdlTestsFile     <- getDataFileName "tests/fdl.json"
  yourTestsFile    <- getDataFileName "tests/yourTests.json"

  anfTests       <- readTests sc anfTestsFile
  dynamicTests   <- readTests sc dynamicTestsFile
  staticTests    <- readTests sc staticTestsFile
  diamondTests   <- readTests sc diamondTestsFile
  adderTests <- readTests sc testsFile
  boaTests   <- readTests sc boaTestsFile
  cobraTests <- readTests sc cobraTestsFile
  eggTests <-  readTests sc eggTestsFile
  fdlTests <-  readTests sc fdlTestsFile
  yourTests  <- readTests sc yourTestsFile
 
  let tests = testGroup "Tests" $
                [testGroup "Normalizer"     anfTests
                ,testGroup "Adder"          adderTests
                , testGroup "Boa"           boaTests
                , testGroup "Cobra"         cobraTests
                , testGroup "Dynamic"       dynamicTests
                , testGroup "static"        staticTests
                , testGroup "Diamondback"   diamondTests
                , testGroup "Egg-eater"     eggTests
                , testGroup "Fer-de-lance"  fdlTests
                , testGroup "Your-Tests"    yourTests
                ]
  defaultMain tests `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

readTests     :: Score -> FilePath -> IO [TestTree]
readTests sc f = map (createTestTree sc) <$> parseTestFile f
