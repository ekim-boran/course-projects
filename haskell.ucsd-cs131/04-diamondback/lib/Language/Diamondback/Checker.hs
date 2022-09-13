{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------
module Language.Diamondback.Checker
  ( -- * Top-level Static Checker
    check

    -- * Error Constructors
  , errUnboundVar
  , errUnboundFun
  ) where

import           Control.Exception
import qualified Data.List                     as L
import           Language.Diamondback.Types
import           Language.Diamondback.Utils
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
check :: BareProgram -> BareProgram
--------------------------------------------------------------------------------
check p = case wellFormed p of
  [] -> p
  es -> throw es

-- | Map from function name to arity
type FunEnv = Env

--------------------------------------------------------------------------------
-- | `wellFormed p` returns the list of errors for a program `p`
--------------------------------------------------------------------------------
wellFormed :: BareProgram -> [UserError]
--------------------------------------------------------------------------------
wellFormed (Prog ds e) = errs
 where
  errs =
    duplicateFunErrors ds
      ++ concatMap (wellFormedD fEnv) ds
      ++ wellFormedE fEnv emptyEnv e

  fEnv = fromListEnv [ (bindId f, length xs) | Decl f xs _ _ <- ds ]

--------------------------------------------------------------------------------
-- | `wellFormedD fEnv vEnv d` returns the list of errors for a func-decl `d`
--------------------------------------------------------------------------------
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedD fEnv (Decl _ xs e _) = wellFormedE fEnv env e ++ dupParam
 where
  dupParam = fmap errDupParam $ concat $ dupBy bindId xs
  env      = fromListEnv [ (bindId x, 0) | x <- xs ]

--------------------------------------------------------------------------------
-- | `wellFormedE vEnv e` returns the list of errors for an expression `e`
--------------------------------------------------------------------------------
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
wellFormedE fEnv e expr = fcs ++ binderrs where
  fcs      = funCallE fEnv expr
  binderrs = bindErr e expr

bindErr _   (Number i  l) = [ errLargeNum l i | i >= maxInt ]
bindErr env (Id     id l) = case lookupEnv id env of
  Nothing  -> [errUnboundVar l id]
  (Just x) -> []
bindErr env (Let id e1 e2 l) = case lookupEnv (bindId id) env of
  Nothing  -> bindErr env e1 ++ bindErr (addEnv id env) e2
  (Just x) -> [errDupBind id]
bindErr env (Prim1 o v l    ) = [ e | e <- bindErr env v ]
bindErr env (Prim2 o v1 v2 l) = [ e | v <- [v1, v2], e <- bindErr env v ]
bindErr env (If    v e1 e2 l) = [ e | v <- [v, e1, e2], e <- bindErr env v ]
bindErr env (App f vs l     ) = [ e | v <- vs, e <- bindErr env v ]
bindErr env _                 = []


funCallE :: Env -> Expr SourceSpan -> [UserError]
funCallE fEnv (App id args l) = case lookupEnv id fEnv of
  Nothing  -> [errUnboundFun l id]
  (Just x) -> [ errCallArity l id | length args /= x ]
funCallE _ _ = []

--------------------------------------------------------------------------------
-- | Error Checkers: In each case, return an empty list if no errors.
--------------------------------------------------------------------------------
duplicateFunErrors :: [BareDecl] -> [UserError]
duplicateFunErrors = fmap errDupFun . concat . dupBy (bindId . fName)

-- | `maxInt` is the largest number you can represent with 63 bits (accounting for sign
--    and the tag bit.

maxInt :: Integer
maxInt = 4611686018427387903
--------------------------------------------------------------------------------
-- | Error Constructors: Use these functions to construct `UserError` values
--   when the corresponding situation arises. e.g see how `errDupFun` is used.
--------------------------------------------------------------------------------

errDupFun :: (Located (Bind a)) => Decl a -> UserError
errDupFun d = mkError (printf "duplicate function '%s'" (pprint f))
                      (sourceSpan f)
  where f = fName d

errDupParam :: (Located (Bind a)) => Bind a -> UserError
errDupParam x =
  mkError (printf "Duplicate parameter '%s'" (bindId x)) (sourceSpan x)

errDupBind :: (Located (Bind a)) => Bind a -> UserError
errDupBind x = mkError (printf "Shadow binding '%s'" (bindId x)) (sourceSpan x)

errLargeNum :: SourceSpan -> Integer -> UserError
errLargeNum l n = mkError (printf "Number '%d' is too large" n) l

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l

errUnboundFun :: SourceSpan -> Id -> UserError
errUnboundFun l f = mkError (printf "Function '%s' is not defined" f) l

errCallArity :: SourceSpan -> Id -> UserError
errCallArity l f =
  mkError (printf "Wrong arity of arguments at call of %s" f) l
