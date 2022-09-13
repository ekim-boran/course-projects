{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Test.Tasty
import Text.Printf
import Control.Exception
import System.Exit
import Language.Cobra.Types      hiding (Result)
import Paths_cobra

main :: IO ()
main = do
  sc <- initScore

  testsFile        <- getDataFileName "tests/adder.json"
  boaTestsFile     <- getDataFileName "tests/boa.json"
  cobraTestsFile   <- getDataFileName "tests/cobra.json"
  anfTestsFile     <- getDataFileName "tests/anf.json"
  dynamicTestsFile <- getDataFileName "tests/dynamic.json"
  yourTestsFile    <- getDataFileName "tests/yourTests.json"

  anfTests       <- readTests sc anfTestsFile
  dynamicTests   <- readTests sc dynamicTestsFile

  adderTests <- readTests sc testsFile
  boaTests   <- readTests sc boaTestsFile
  cobraTests <- readTests sc cobraTestsFile
  yourTests  <- readTests sc yourTestsFile

  let tests = testGroup "Tests" $
                [ testGroup "Normalizer"      anfTests
                , testGroup "Adder"           adderTests
                , testGroup "Boa"             boaTests
                , testGroup "Cobra"           cobraTests
                , testGroup "Dynamic"         dynamicTests
                , testGroup "Your-Tests"      yourTests
                ]
  defaultMain tests `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

readTests     :: Score -> FilePath -> IO [TestTree]
readTests sc f = map (createTestTree sc) <$> parseTestFile f
