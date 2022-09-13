module HW6.HW6 where

import Control.Applicative (Alternative (..))
import Control.Monad ()
import qualified Data.Bool as PP
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (foldl1')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified HW6.Parser as P
import qualified HW6.ParserCombinators as P
import HW6.State (State)
import qualified HW6.State as S
import Text.PrettyPrint (Doc, fsep, hsep, vcat, ($$), ($+$), (<+>))
import qualified Text.PrettyPrint as PP

main :: IO ()
main = return ()

-------------------------------------------------------------------------

type Variable = String

newtype Block
  = Block [Statement] -- { s1; ... sn; }
  deriving (Eq, Show)

data Statement
  = Assign Variable Expression -- x = e
  | If Expression Block Block -- if e then s1 else s2
  | While Expression Block -- while e do s
  deriving (Eq, Show)

data Expression
  = Var Variable -- x
  | Val Value -- v
  | Op Expression Bop Expression
  deriving (Eq, Show)

data Bop
  = Plus -- +  :: Int -> Int -> Int
  | Minus -- -  :: Int -> Int -> Int
  | Times --  :: Int -> Int -> Int
  | Divide -- /  :: Int -> Int -> Int
  | Gt -- >  :: Int -> Int -> Bool
  | Ge -- >= :: Int -> Int -> Bool
  | Lt -- <  :: Int -> Int -> Bool
  | Le -- <= :: Int -> Int -> Bool
  deriving (Eq, Show, Enum)

data Value
  = IntVal Int
  | BoolVal Bool
  deriving (Eq, Show)

type Store = Map Variable Value

-------------------------------------------------------------------------
-- Here are some test programs. You can ignore the 80-column limit for this part
-- of the file.

-- test.imp
wTest :: Block
wTest =
  Block
    [ Assign "x" (Op (Op (Op (Val (IntVal 1)) Plus (Val (IntVal 2))) Minus (Val (IntVal 3))) Plus (Op (Val (IntVal 1)) Plus (Val (IntVal 3)))),
      Assign "y" (Val (IntVal 0)),
      While
        (Op (Var "x") Gt (Val (IntVal 0)))
        ( Block
            [ Assign "y" (Op (Var "y") Plus (Var "x")),
              Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))
            ]
        )
    ]

-- fact.imp
wFact :: Block
wFact =
  Block
    [ Assign "n" (Val (IntVal 5)),
      Assign "f" (Val (IntVal 1)),
      While
        (Op (Var "n") Gt (Val (IntVal 0)))
        ( Block
            [ Assign "x" (Var "n"),
              Assign "z" (Var "f"),
              While
                (Op (Var "x") Gt (Val (IntVal 1)))
                ( Block
                    [ Assign "f" (Op (Var "z") Plus (Var "f")),
                      Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))
                    ]
                ),
              Assign "n" (Op (Var "n") Minus (Val (IntVal 1)))
            ]
        )
    ]

-- abs.imp
wAbs :: Block
wAbs =
  Block
    [ Assign "x" (Op (Val (IntVal 0)) Minus (Val (IntVal 3))),
      If
        (Op (Var "x") Lt (Val (IntVal 0)))
        (Block [Assign "x" (Op (Val (IntVal 0)) Minus (Var "x"))])
        (Block [])
    ]

-- times.imp
wTimes :: Block
wTimes =
  Block
    [ Assign "x" (Val (IntVal 10)),
      Assign "y" (Val (IntVal 3)),
      Assign "z" (Val (IntVal 0)),
      While
        (Op (Var "x") Gt (Val (IntVal 0)))
        ( Block
            [ Assign "z" (Op (Var "z") Plus (Var "y")),
              Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))
            ]
        )
    ]

-------------------------------------------------------------------------

evalOp b (IntVal i) (IntVal j) = case b of
  Plus -> IntVal $ i + j
  Minus -> IntVal $ i - j
  Times -> IntVal $ i * j
  Divide -> if j == 0 then IntVal 0 else IntVal $ i `div` j
  Gt -> BoolVal $ i > j
  Ge -> BoolVal $ i >= j
  Lt -> BoolVal $ i < j
  Le -> BoolVal $ i <= j
evalOp b _ _ = BoolVal False

evalE :: Expression -> State Store Value
evalE (Var s) = S.gets (fromMaybe (IntVal 0) . Map.lookup s)
evalE (Val x) = pure x
evalE (Op l b r) = evalOp b <$> evalE l <*> evalE r

evalS :: Statement -> State Store ()
evalS x@(While e block) = do
  e <- evalE e
  case e of
    (BoolVal True) -> eval block >> evalS x
    _ -> return ()
evalS (Assign x e) = evalE e >>= (S.modify . Map.insert x)
evalS (If e b1 b2) = do
  e <- evalE e
  case e of
    (BoolVal True) -> eval b1
    _ -> eval b2

eval :: Block -> State Store ()
eval (Block st) = do
  traverse_ evalS st

exec :: Block -> Store -> Store
exec b = S.execState (eval b)

run :: Block -> IO ()
run block = do
  putStrLn "Output Store:"
  print (exec block Map.empty)

---------------------------------------------

class PP a where
  pp :: a -> Doc
  pp a = ppPrec 0 a
  ppPrec :: Int -> a -> Doc
  ppPrec _ x = pp x

instance PP Bop where
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Divide = PP.char '/'
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="

oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

indented :: PP a => a -> String
indented = PP.render . pp

instance PP Value where
  pp (IntVal i) = PP.int i
  pp (BoolVal i) = PP.text (toLower <$> show i)

instance PP Expression where
  ppPrec _ (Var s) = PP.text s
  ppPrec _ (Val s) = pp s
  ppPrec p (Op l b r)
    | p > level b = PP.parens (ppPrec (level b) l <+> pp b <+> ppPrec (1 + level b) r)
    | otherwise = ppPrec (level b) l <+> pp b <+> ppPrec (1 + level b) r

instance PP Block where
  pp (Block xs) = vcat $ (pp <$> xs)

block before b@(Block xs) = before <+> PP.text "{" $$ inside
  where
    inside = if null xs then PP.nest (-100) (PP.text "}") else PP.nest 2 (pp b) $$ PP.text "}"

instance PP Statement where
  pp (While e b) = block (PP.text "while" <+> PP.parens (pp e)) b
  pp (If e b1 b2) = block (PP.text "if" <+> PP.parens (pp e)) b1 <+> block (PP.text "else") b2
  pp (Assign x e) = PP.text x <+> PP.text "=" <+> (pp e <> PP.text ";")

-- use the C++ precendence level table
level :: Bop -> Int
level Times = 7
level Divide = 7
level Plus = 5
level Minus = 5
level _ = 3 -- comparison operators

------------------------------------------------------------------------

step :: Block -> State Store Block
step (Block ((If e (Block b1s) (Block b2s)) : xs)) = do
  r <- evalE e
  case r of
    BoolVal True -> return (Block (b1s ++ xs))
    _ -> return (Block (b2s ++ xs))
step (Block (x@(While e (Block bs)) : xs)) = do
  r <- evalE e
  case r of
    BoolVal True -> return (Block (bs ++ [x] ++ xs))
    _ -> return (Block xs)
step (Block (x : xs)) = evalS x >> return (Block xs)
step (Block []) = return (Block [])

-- | Is this block completely evaluated?
final :: Block -> Bool
final (Block []) = True
final _ = False

-- | Evaluate this block to completion
execStep :: Block -> Store -> Store
execStep b = S.execState (eval b)

-- | Evaluate this block for a specified number of steps
boundedStep :: Int -> Block -> State Store Block
boundedStep n b | n <= 0 = return b
boundedStep n b = step b >>= boundedStep (n - 1)

stepper :: Block -> IO ()
stepper b = go [] (b, Map.empty)
  where
    go history (block, store) = do
      putBlock block
      putStr "imp> "
      str <- getLine
      let printLine s = print s >> go history (block, store)
      case str of
        "x" -> return () -- quit the stepper
        "n" -> go ((block, store) : history) $ S.runState (step block) store
        "b" -> case history of
          [] -> printLine "?"
          (x : xs) -> go xs x
        'v' : ' ' : xs -> case Map.lookup xs store of
          Nothing -> printLine "Unknown var"
          (Just x) -> printLine $ PP.render (pp x)
        _ -> printLine "?"
    putBlock :: Block -> IO ()
    putBlock (Block []) = putStrLn "done"
    putBlock (Block (s : _)) = putStr "-->" >> putStrLn (PP.render (pp s))

------------------------------------------------------------------------

valueP :: P.Parser Value
valueP = intP <|> boolP

intP :: P.Parser Value
intP = IntVal <$> P.int

constP :: String -> a -> P.Parser a
constP s a = P.string s $> a

boolP :: P.Parser Value
boolP = constP "false" (BoolVal False) <|> constP "true" (BoolVal True)

opP :: P.Parser Bop
opP =
  constP "+" Plus
    <|> constP "-" Minus
    <|> constP "*" Times
    <|> constP "/" Divide
    <|> constP ">" Gt
    <|> constP ">=" Ge
    <|> constP "<" Lt
    <|> constP "<=" Le

varP :: P.Parser Variable
varP = some P.lower

wsP :: P.Parser a -> P.Parser a
wsP p = many P.space *> p <* many P.space

h a b = flip Op <$> (constP a b)

exprP = foldr (flip P.chainl1) term xs
  where
    xs =
      [ P.choice
          [ h "<=" Le,
            h "<" Lt,
            h ">=" Ge,
            h ">" Gt
          ],
        P.choice
          [ h "+" Plus,
            h "-" Minus
          ],
        P.choice
          [ h "/" Divide,
            h "*" Times
          ]
      ]

term =
  Val <$> wsP valueP
    <|> Var <$> wsP varP
    <|> wsP (btwChar '(' ')' exprP)

-- >>> P.doParse exprP "1 + 2 + 4"
-- [(Op (Op (Val (IntVal 1)) Plus (Val (IntVal 2))) Plus (Val (IntVal 4)),"")]
--

keyword s = wsP (P.string s)

btwChar c1 c2 p = P.between (wsP $ P.char c1) p (wsP $ P.char c2)

whileP = While <$> (keyword "while" *> (btwChar '(' ')' exprP)) <*> btwChar '{' '}' toplevelP

ifp = If <$> (keyword "if" *> btwChar '(' ')' exprP) <*> btwChar '{' '}' toplevelP <*> (keyword "else" *> btwChar '{' '}' toplevelP)

assignP = Assign <$> (wsP varP <* keyword "=") <*> (wsP exprP <* keyword ";")

statementP :: P.Parser Statement
statementP = whileP <|> ifp <|> assignP

toplevelP :: P.Parser Block
toplevelP = Block <$> many statementP
