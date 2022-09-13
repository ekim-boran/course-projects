{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( defsFile
  , execFile
  , execString
  , execEnvString
  , execExpr
  , eval
  , lookupId
  , prelude
  , parse
  , env0
  ) where

import           Control.Exception              ( catch
                                                , throw
                                                )
import           Control.Monad.Fix
import qualified Language.Nano.Parser          as Parser
import           Language.Nano.Types
import           System.Directory               ( doesFileExist )
--------------------------------------------------------------------------------
defsFile :: FilePath -> IO [(Id, Expr)]
--------------------------------------------------------------------------------
defsFile f = safeRead f >>= return . Parser.parseDefs

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (safeRead f >>= execString) `catch` exitError

safeRead :: FilePath -> IO String
safeRead f = do
  b <- doesFileExist f
  if b then readFile f else throw (Error ("unknown file: " ++ f))

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parse s) `catch` exitError

--------------------------------------------------------------------------------
execEnvString :: Env -> String -> IO Value
--------------------------------------------------------------------------------
execEnvString env s = return (eval env (parse s)) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = Parser.parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus "x" "y") (EBin Plus "z" "z1"))
-- 0
--
-- >>> eval env0 "p"
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt "z1" "x") (EBin Ne "y" "z") (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq "z1" "x") (EBin Le "y" "z") (EBin Le "z" "y")
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus "x" "y"
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus "x" "x")) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus "x" "y")) (EApp "f" "h")
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp "g" (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq "n" (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul "n" (EApp "fac" (EBin Minus "n" (EInt 1))))))
--             (EApp "fac" (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp "head" el)
-- 1
-- >>> execExpr (EApp "tail" el)
-- (2 : [])

--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
eval env e = case evalE env e of
  Left  exn -> exn
  Right val -> val

--------------------------------------------------------------------------------
evalE :: Env -> Expr -> Either Value Value
--------------------------------------------------------------------------------
evalE env (EInt  i)      = return $ VInt i
evalE env (EBool b)      = return $ VBool b
evalE env ENil           = return VNil

evalE env (EVar x      ) = return $ lookupId x env
evalE env (EBin o e1 e2) = do
  a <- evalE env e1
  b <- evalE env e2
  return $ evalOp o a b

evalE env (EIf c t e) = do
  ce <- evalE env c
  case ce of
    (VBool True ) -> evalE env t
    (VBool False) -> evalE env e
    _             -> throw $ Error "if type error"

evalE env (ELet x e1 e2) = do
  a <- mfix (\a -> evalE ((x, a) : env) e1)
  evalE ((x, a) : env) e2

  --Right $ let new_env = (x, eval new_env e1) : env in eval new_env e2

evalE env (EApp e1 e2) = do
  ee1 <- evalE env e1
  case ee1 of
    (VClos env' id e1) -> evalE env e2 >>= \ne -> evalE ((id, ne) : env') e1
    (VPrim f         ) -> evalE env e2 >>= (return . f)
    _                  -> throw $ Error "apply type error"

evalE env (ELam x e1   ) = Right $ VClos env x e1

evalE env (EThr e      ) = Left $ eval env e
evalE env (ETry e1 x e2) = case evalE env e1 of
  (Left  ex) -> evalE ((x, ex) : env) e2
  (Right v ) -> Right v




--------------------------------------------------------------------------------
-- | Unit tests for `throw`
--------------------------------------------------------------------------------

-- 1 + 2                 ==> 3
-- (throw 1) + 2         ==> 1
-- 1 + (throw 2)         ==> 2
-- (throw 1) + (throw 2) ==> 1
-- throw (1 + 2)         ==> 3
-- throw (1 + (throw 2)) ==> 2

-- >>> let ex_1_2   = EBin Plus (EInt 1) (EInt 2)
-- >>> let ex_t1_2  = EBin Plus (EThr (EInt 1)) (EInt 2)
-- >>> let ex_1_t2  = EBin Plus (EInt 1) (EThr (EInt 2)) 
-- >>> let ex_t1_t2 = EBin Plus (EThr (EInt 1)) (EThr (EInt 2)) 
-- >>> let ex_t12   = EThr (EBin Plus (EInt 1) (EInt 2))
-- >>> let ex_tt12  = EThr (EBin Plus (EInt 1) (EThr (EInt 2)))
-- >>> eval [] ex_1_2 
-- 3
--
-- >>> eval [] ex_t1_2 
-- 1
-- >>> eval [] ex_1_t2 
-- 2
-- >>> eval [] ex_t1_t2 
-- 1
-- >>> eval [] ex_t12 
-- 3
-- >>> eval [] ex_tt12 
-- 2

--------------------------------------------------------------------------------
-- | Unit tests for `try-catch`
--------------------------------------------------------------------------------
-- try (1 + 2)         handle z => z + 10   ==> 3 
-- try ((throw 1) + 2) handle z => z + 10   ==> 11 
-- try (1 + (throw 2)) handle z => z + 10   ==> 12 
-- try ((throw 1) + (throw 2)) handle z => z + 10   ==> 11 

-- >>> eval [] (ETry ex_1_2 "z" (EBin Plus "z" (EInt 10))
-- 3
-- >>> eval [] (ETry ex_t1_2 "z" (EBin Plus "z" (EInt 10))
-- 11
-- >>> eval [] (ETry ex_1_t2 "z" (EBin Plus "z" (EInt 10))
-- 12
-- >>> eval [] (ETry ex_t1_t2 "z" (EBin Plus "z" (EInt 10))
-- 11
-- >>> eval [] (ETry ex_t12 "z" (EBin Plus "z" (EInt 10))
-- 13
-- >>> eval [] (ETry ex_tt12 "z" (EBin Plus "z" (EInt 10))
-- 12

 --------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus  (VInt  a)   (VInt  b)   = VInt (a + b)
evalOp Minus (VInt  a)   (VInt  b)   = VInt (a - b)
evalOp Mul   (VInt  a)   (VInt  b)   = VInt (a * b)
evalOp Div   (VInt  a)   (VInt  b)   = VInt (a `div` b)

evalOp Lt    (VInt  a)   (VInt  b)   = VBool (a < b)
evalOp Le    (VInt  a)   (VInt  b)   = VBool (a <= b)
evalOp Eq    (VInt  a)   (VInt  b)   = VBool (a == b)

evalOp Lt    (VBool a)   (VBool b)   = VBool (a < b)
evalOp Le    (VBool a)   (VBool b)   = VBool (a <= b)
evalOp Eq    (VBool a)   (VBool b)   = VBool (a == b)

evalOp And   (VBool a)   (VBool b)   = VBool (a && b)
evalOp Or    (VBool a)   (VBool b)   = VBool (a || b)
evalOp Cons  a           b           = VPair a b


evalOp Eq    VNil        VNil        = VBool True
evalOp Eq    VNil        (VPair _ _) = VBool False
evalOp Eq    (VPair _ _) VNil        = VBool False
evalOp Eq (VPair a b) (VPair c d) = evalOp And (evalOp Eq a c) (evalOp Eq b d)


evalOp Ne    x           y           = case evalOp Eq x y of
  VBool True  -> VBool False
  VBool False -> VBool True
  _           -> throw $ Error "type error"


evalOp op _ _ = throw $ Error (show op ++ " operator type error")


--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId id env = case lookup id env of
  Nothing  -> throw $ Error $ "unbound variable: " ++ id
  (Just c) -> c


head' (VPair a _) = a
head' _           = throw $ Error "list type error"

tail' (VPair _ b) = b
tail' _           = throw $ Error "list type error"
prelude :: Env
prelude = [("head", VPrim head'), ("tail", VPrim tail')]



env0 :: Env
env0 =
  [("z1", VInt 0), ("x", VInt 1), ("y", VInt 2), ("z", VInt 3), ("z1", VInt 4)]

--------------------------------------------------------------------------------
