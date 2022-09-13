
import Data.Vect
import Data.String
import System.File
import System
import Control.Monad.State 
data Token = Number Int | Operator (Int -> Int -> Int) | Read | Write

parseToken :  String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken "r" = Just (Read)
parseToken "p" = Just Write
parseToken str = map Number (parseInteger (trim str))
 


tokenise :  String -> Maybe (List Token)
tokenise = traverse parseToken . words


pop : (MonadState (List Int) m ) =>  m (Maybe Int)
pop =  do 
    xs <- get
    case xs of
        [] => pure $ Nothing
        (x::xs) => pure $ (xs, Just x)

--push : Int -> Calc ()
--push i = C $ \xs => Just (i :: xs, ())
--
--
--implementation Functor Calc where
--  map f (C sa) = C $ \s => case sa s of
--    Nothing      => Nothing
--    Just (s', a) => Just (s', f a)
--
--implementation Applicative Calc where
--  pure x = C (\s => Just (s, x))
--  C sf <*> C sx = C $ \s => case sf s of
--    Nothing      => Nothing
--    Just (s', f) => case sx s' of
--      Nothing       => Nothing
--      Just (s'', x) => Just (s'', f x)
--
--implementation Monad Calc where
--  C sa >>= f = C $ \s => case sa s of
--    Nothing      => Nothing
--    Just (s', a) => unwrapCalc (f a) s'
--    where 
--    unwrapCalc : Calc b -> (List Int -> Maybe (List Int, b))
--    unwrapCalc (C a) = a
--
--
--action :  Token -> Calc ()
--action (Number   a) = push a
--action (Operator f) = do
--  v <- (flip f) <$> pop <*> pop
--  push v
--
--evaluate :  List Token -> Calc Int
--evaluate ts = (traverse action ts) >>= (\_ => pop)
--
--runCalc :  Calc b -> Maybe b
--runCalc (C f) = map snd (f [])
--
--
--eval :  String -> Maybe Int
--eval = (>>= runCalc) . map evaluate . tokenise
--
-- 
--
--main : IO ()
--main = do 
--    args <- getArgs 
--    case args of 
--        (x::y::xs) =>  do
--            c <-  readFile y 
--            case c of 
--                Left _ => pure ()
--                Right contents =>   putStrLn (show $ eval contents) 
--        _ => pure()
--   
--
----act (Number n)  = push n
----act (Operator o) = 
--
----echo 3 5 + > prog && ./build/exec/interpret prog