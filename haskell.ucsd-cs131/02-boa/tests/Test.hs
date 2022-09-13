{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Test.Tasty
import Text.Printf
import Control.Exception
import System.Exit
import Paths_boa

main :: IO ()
main = do
  sc <- initScore

  testsFile     <- getDataFileName "tests/adder.json"
  boaTestsFile  <- getDataFileName "tests/boa.json"
  anfTestsFile  <- getDataFileName "tests/anf.json"
  yourTestsFile <- getDataFileName "tests/yourTests.json"

  anfTests   <- readTests sc anfTestsFile

  adderTests <- readTests sc testsFile
  boaTests   <- readTests sc boaTestsFile
  yourTests  <- readTests sc yourTestsFile

  let tests = testGroup "Tests" $
                [ testGroup "Normalizer"      anfTests
                , testGroup "Adder"           adderTests
                , testGroup "Boa"             boaTests
                , testGroup "Your-Tests"      yourTests
                ]
  defaultMain tests `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

readTests     :: Score -> FilePath -> IO [TestTree]
readTests sc f = map (createTestTree sc) <$> parseTestFile f

