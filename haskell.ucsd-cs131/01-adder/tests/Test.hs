{-# LANGUAGE ScopedTypeVariables #-}

import           Common
import           Control.Exception
import           System.Exit
import           Test.Tasty
import           Text.Printf
import           Paths_adder

main :: IO ()
main = do
  sc <- initScore
  testsFile     <- getDataFileName "tests/tests.json"
  yourTestsFile <- getDataFileName "tests/yourTests.json"
  adderTests <- readTests sc testsFile
  yourTests  <- readTests sc yourTestsFile
  let tests = testGroup "Tests" [ testGroup "Adder"      adderTests
                                , testGroup "Your-Tests" yourTests
                                ]
  defaultMain tests `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

readTests     :: Score -> FilePath -> IO [TestTree]
readTests sc f = map (createTestTree sc) <$> parseTestFile f
