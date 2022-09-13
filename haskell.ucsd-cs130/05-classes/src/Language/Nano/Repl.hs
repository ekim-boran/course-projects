-- | This module has various "utilities" that you can use to build a REPL. 

module Language.Nano.Repl where

import           Control.Exception
import qualified Data.Char                     as Char
import qualified Data.List                     as L
import           Language.Nano.Eval
import           Language.Nano.Types
import           System.Exit
import           System.IO

--------------------------------------------------------------------------------
welcome :: String
--------------------------------------------------------------------------------
welcome = unlines
  [ "------------------------------------------------------------"
  , "-------- The NANO Interpreter v.0.0.0.0 --------------------"
  , "------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------
putStrFlush :: String -> IO ()
--------------------------------------------------------------------------------
putStrFlush str = do
  putStr str
  hFlush stdout

--------------------------------------------------------------------------------
doQuit :: IO a
--------------------------------------------------------------------------------
doQuit = do
  putStrLn "Goodbye."
  exitWith ExitSuccess

--------------------------------------------------------------------------------
doEval :: Env -> String -> IO ()
--------------------------------------------------------------------------------
doEval env s = (print =<< execEnvString env s) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doUnknown :: IO ()
--------------------------------------------------------------------------------
doUnknown = putStrLn "I'm sorry Dave, I'm sorry I can't do that..."

--------------------------------------------------------------------------------
doRun :: FilePath -> IO ()
--------------------------------------------------------------------------------
doRun f = (print =<< execFile f) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doLoad :: FilePath -> IO Env
--------------------------------------------------------------------------------
doLoad f = (defsEnv =<< defsFile f) `catch` exitEnv

exitEnv :: Error -> IO Env
exitEnv err = putStrLn (errMsg err) >> return prelude


--------------------------------------------------------------------------------
-- HINT: You may want to implement `defsEnv` and then use `doLoad`
--------------------------------------------------------------------------------
defsEnv :: [(Id, Expr)] -> IO Env
--------------------------------------------------------------------------------
defsEnv e = do
  return t
 where
  t = reverse $ foldl f prelude e
  f env (id, expr) = (id, eval env expr) : env
--------------------------------------------------------------------------------
-- | A Datatype Commands for the shell -----------------------------------------
--------------------------------------------------------------------------------

data Cmd
  = CEval String    -- ^ `CEval s` means parse-and-evaluate the `s`
  | CRun  FilePath  -- ^ `CRun f`  means parse-and-evaluate the "top" binder of `f`
  | CLoad FilePath  -- ^ `CLoad f` means parse-and-add-to-env all the binders of `f`
  | CQuit           -- ^ `CQuit`   means exit the shell
  | CUnknown        -- ^ any other unknown command
  deriving (Show)




strCmd' :: String -> Cmd
strCmd' str | pfxRun `L.isPrefixOf` str  = CRun (chomp (length pfxRun) str)
            | pfxLoad `L.isPrefixOf` str = CLoad (chomp (length pfxLoad) str)
            | pfxQuit `L.isPrefixOf` str = CQuit
            | otherwise                  = CUnknown

strCmd str | ":" `L.isPrefixOf` str = strCmd' (chomp 1 str)
           | otherwise              = CEval str

chomp :: Int -> String -> String
chomp n s = dropWhile Char.isSpace (drop n s)

pfxRun, pfxLoad, pfxQuit :: String
pfxRun = "run"
pfxLoad = "load"
pfxQuit = "quit"


