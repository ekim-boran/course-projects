{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import Data.Aeson hiding (Result)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import Data.IORef
import Data.List (isInfixOf)
import qualified Data.Text as T
import Language.Adder.Types hiding (Result)
import Language.Adder.Utils
import System.Exit
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

overflowError = Left "Error: arithmetic overflow"

rLines = Right . unlines

dynamicError t = Left ("Error: expected a " ++ pprint t)

staticError = Left

--------------------------------------------------------------------------------
run :: FilePath -> Program -> IO Result
--------------------------------------------------------------------------------
run name pgm = do
  _ <- generateSource name pgm -- generate source file
  r <- executeShellCommand logF cmd timeLimit -- compile & run
  readResult resF logF r
  where
    cmd = printf "make %s" resF
    resF = dirExt "output" name Res
    logF = dirExt "output" name Log

-- | `timeLimit` for each test is 15 seconds
timeLimit :: Int
timeLimit = 15 * (10 ^ 6)

--------------------------------------------------------------------------------
generateSource :: FilePath -> Program -> IO ()
--------------------------------------------------------------------------------
generateSource _ File = return ()
generateSource name (Code pgm) = writeFile srcF pgm
  where
    srcF = dirExt "input" name Src

--------------------------------------------------------------------------------
readResult :: FilePath -> FilePath -> ExitCode -> IO Result
--------------------------------------------------------------------------------
readResult resF _ ExitSuccess = Right <$> readFile resF
readResult _ _ (ExitFailure 100) = Left <$> return "TIMEOUT!"
readResult _ logF (ExitFailure _) = Left <$> readFile logF

dirExt :: FilePath -> FilePath -> Ext -> FilePath
dirExt dir name e = "tests" </> dir </> name `ext` e

--------------------------------------------------------------------------------

-- | A test program is either a filename or a text representation of source

--------------------------------------------------------------------------------
data Program = File | Code Text deriving (Show)

type Result = Either Text Text

--------------------------------------------------------------------------------

-- | Construct a single compiler test case from a `Program`

--------------------------------------------------------------------------------
mkTest :: Score -> String -> Program -> Result -> TestTree
mkTest sc name pgm = mkTest' sc 1 name (run name pgm)

mkTest' :: Score -> Int -> String -> IO Result -> Result -> TestTree
mkTest' sc n name act expect = testCase name $ do
  updateTotal sc n
  res <- act
  check sc n res expect

--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    if f x == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

-- check :: Result -> Result -> TestTree
check sc n (Right resV) (Right expectV)
  | trim expectV == trim resV = updateCurrent sc n
  | otherwise = assertFailure "Wrong result"
check sc n (Left resE) (Left expectE)
  | matchError expectE resE = updateCurrent sc n
  | otherwise = assertFailure "Wrong error"
check _ _ (Left resE) (Right expectV) = assertEqual "Unexpected error" ("Value " ++ expectV) ("Error " ++ resE)
check _ _ (Right resV) (Left expectE) = assertEqual "Unexpected result" ("Error " ++ expectE) ("Value " ++ resV)

matchError expectE resE = tx expectE `isInfixOf` tx resE
  where
    tx = map toLower

--------------------------------------------------------------------------------
type Score = IORef (Int, Int)

--------------------------------------------------------------------------------

getTotal :: Score -> IO (Int, Int)
getTotal = readIORef

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)

--------------------------------------------------------------------------------

-- | Tests

--------------------------------------------------------------------------------

-- { name   : string
-- , code   : "file" | string
-- , result : { value : string } | { failure : string }
-- }

data TestResult
  = Value String
  | Failure String
  deriving (Show)

data Test = Test
  { testName :: String,
    testCode :: Program,
    testResult :: TestResult
  }
  deriving (Show)

instance FromJSON Program where
  parseJSON = withText "\"file\" or String" (return . helper . T.unpack)
    where
      helper s = if s == "file" then File else Code s

instance FromJSON TestResult where
  parseJSON = withObject expected $ \obj -> helper (KM.toList obj) obj
    where
      expected = "{ \"value\": String } or { \"failure\" : String }"
      helper [("value", val)] _ = withText "String" (return . Value . T.unpack) val
      helper [("failure", err)] _ = withText "String" (return . Failure . T.unpack) err
      helper _ obj = typeMismatch expected (Object obj)

instance FromJSON Test where
  parseJSON = withObject expected p
    where
      p o =
        if length (KM.keys o) == 3
          then Test <$> o .: "name" <*> o .: "code" <*> o .: "result"
          else typeMismatch expected (Object o)
      expected = "{ \"name\" : ..., \"code\" : ..., \"result\" : ... }"

createTestTree :: Score -> Test -> TestTree
createTestTree sc Test {..} =
  let res = case testResult of
        Value s -> Right s
        Failure s -> Left s
   in mkTest sc testName testCode res

parseTestFile :: FilePath -> IO [Test]
parseTestFile f = do
  b <- B.readFile f
  case eitherDecode b of
    Left err -> fail $ "!!! ERROR IN TEST FILE" ++ f ++ " : " ++ err
    Right res -> return res
