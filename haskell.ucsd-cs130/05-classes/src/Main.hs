import           GHC.IO.Encoding
import qualified Language.Nano.Eval            as Nano
import           Language.Nano.Repl
import qualified Language.Nano.Types           as Nano
import           Text.Printf

import           Data.List                     as L
processCommand env (CEval s) = do
  doEval env s
  return (Just env)

processCommand env (CRun  f) = doRun f >> return (Just env)
processCommand env (CLoad f) = do
  env' <- doLoad f
  putStr "definitions: "
  putStrLn (L.intercalate " " (fmap fst (env ++ env')))
  return $ Just $ env ++ env'
processCommand _   CQuit    = doQuit >> (return Nothing)
processCommand env CUnknown = doUnknown >> return (Just env)


mainLoop count env = do
  command <- fmap strCmd getLine
  putStr $ "λ [" ++ (show count) ++ "] "
  new_env <- processCommand env command
  case new_env of
    Nothing   -> return ()
    (Just ne) -> mainLoop (count + 1) ne

main :: IO ()
main = do
  setLocaleEncoding utf8
  putStrFlush welcome
  putStrLn ""

  mainLoop 0 []
  return ()
 --------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

