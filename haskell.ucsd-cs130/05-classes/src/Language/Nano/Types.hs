{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Types where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)
import qualified Data.List as L

import           Control.Exception
import           Data.Typeable

data Error = Error {errMsg :: String}
             deriving (Show, Typeable)

instance Exception Error

data Binop
  = Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | And
  | Or
  | Cons
  deriving (Eq, Ord, Show)

type Id = String

instance IsString Expr where
  fromString = EVar

data Expr
  = EInt  Int                 -- ^ 0, 1, 2, ...
  | EBool Bool                -- ^ True, False
  | ENil                      -- ^ []
  | EVar Id                   -- ^ x
  | EBin Binop Expr Expr      -- ^ e1 op e2
  | EIf  Expr Expr  Expr      -- ^ if e1 then e2 else e3
  | ELet Id   Expr  Expr      -- ^ let x = e1 in e2
  | EApp Expr Expr            -- ^ (e1 e2)
  | ELam Id   Expr            -- ^ \x -> e  
  | ETry Expr Id    Expr      -- ^ try e1 handle x => e2
  | EThr Expr                 -- ^ throw e 
  deriving (Eq, Show)


data Value
  = VInt  Int
  | VBool Bool
  | VClos Env Id Expr
  | VNil
  | VPair Value Value
  | VErr  String
  | VPrim (Value -> Value)

type Env = [(Id, Value)]

instance Eq Value where
  (VInt x1)     == (VInt x2)     = x1 == x2
  (VBool x1)    == (VBool x2)    = x1 == x2
  VNil          == VNil          = True
  (VPair x1 y1) == (VPair x2 y2) = x1 == x2 && y1 == y2
  _             == _             = False

-- instance Show Binop where
--   show = binopString

instance Show Value where
  show = valueString

-- instance Show Expr where
--   show = exprString

binopString :: Binop -> String
binopString Plus  = "+"
binopString Minus = "-"
binopString Mul   = "*"
binopString Div   = "/"
binopString Eq    = "="
binopString Ne    = "!="
binopString Lt    = "<"
binopString Le    = "<="
binopString And   = "&&"
binopString Or    = "||"
binopString Cons  = ":"

valueString :: Value -> String
valueString (VInt i)        = printf "%d" i
valueString (VBool b)       = printf "%s" (show b)
valueString (VClos env x v) = printf "<<%s, \\%s -> %s>>" (envString env) x (show v)
valueString (VPair v w)     = printf "(%s : %s)" (show v) (show w)
valueString (VErr s)        = printf "ERROR: %s" s
valueString VNil            = "[]"
valueString (VPrim _)       = "<<primitive-function>>"

envString :: Env -> String
envString env = printf "{ %s }" (L.intercalate ", " bs)
  where
    bs        = [ x ++ " := " ++ show v | (x, v) <- env]

exprString :: Expr -> String
exprString (EInt i)       = printf "%d" i
exprString (EBool b)      = printf "%s" (show b)
exprString (EVar x)       = x
exprString (EBin o e1 e2) = printf "(%s %s %s)" (show e1) (show o) (show e2)
exprString (EIf c t e)    = printf "if %s then %s else %s" (show c) (show t) (show e)
exprString (ELet x e e')  = printf "let %s = %s in \n %s" x (show e) (show e')
exprString (EApp e1 e2)   = printf "(%s %s)" (show e1) (show e2)
exprString (ELam x e)     = printf "\\%s -> %s" x (show e)
exprString ENil           = "[]"
exprString (ETry e1 x e2) = printf "try (%s) handle %s => %s" (show e1) x (show e2)
exprString (EThr e)       = printf "(throw %s)" (show e)


--------------------------------------------------------------------------------
class Nano a where
  expr  :: a -> Expr
  value :: a -> Value

instance Nano Int where
  expr  = EInt
  value = VInt

instance Nano Bool where
  expr  = EBool
  value = VBool

exprList :: [Expr] -> Expr
exprList = foldr (EBin Cons) ENil

valueList :: [Value] -> Value
valueList = foldr VPair VNil
