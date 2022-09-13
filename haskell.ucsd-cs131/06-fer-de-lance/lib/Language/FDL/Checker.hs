{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------
module Language.FDL.Checker
  ( -- * Top-level Static Checker
    check

    -- * Error Constructors
  , errUnboundVar
  , errUnboundFun
  ) where

import           Control.Exception
import qualified Data.List                     as L
import           Language.FDL.Types
import           Language.FDL.Utils
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
check :: Bare -> Bare
--------------------------------------------------------------------------------
check p = case wellFormed p of
  [] -> p
  es -> throw es

--------------------------------------------------------------------------------
-- | `wellFormed e` returns the list of errors for an expression `e`
--------------------------------------------------------------------------------
wellFormed :: Bare -> [UserError]
wellFormed = go emptyEnv
 where
  gos = concatMap . go
  go _    Boolean{}          = []
  go _    (Number n l      ) = largeNumberErrors n l
  go vEnv (Id     x l      ) = unboundVarErrors vEnv x l
  go vEnv (Prim1 _ e _     ) = go vEnv e
  go vEnv (Prim2 _  e1 e2 _) = gos vEnv [e1, e2]
  go vEnv (If    e1 e2 e3 _) = gos vEnv [e1, e2, e3]
  go vEnv (Let x e1 e2 _) =
    duplicateBindErrors vEnv x ++ go vEnv e1 ++ go (addEnv x vEnv) e2
  go vEnv (Tuple es _     ) = gos vEnv es
  go vEnv (GetItem e1 e2 _) = gos vEnv [e1, e2]

  go vEnv (App     f  es l) = gos vEnv (f : es)
  go vEnv (Lam     xs e  _) = duplicateParamErrors xs ++ go (addsEnv xs vEnv) e
  go vEnv (Fun f xs e _) =
    duplicateParamErrors xs ++ go (addsEnv (f : xs) vEnv) e





addsEnv :: [BareBind] -> Env -> Env
addsEnv xs env = L.foldl' (\env x -> addEnv x env) env xs

--------------------------------------------------------------------------------
-- | Error Checkers: In each case, return an empty list if no errors.
--------------------------------------------------------------------------------
duplicateParamErrors :: [BareBind] -> [UserError]
duplicateParamErrors = map (errDupParam . head) . dupBy bindId

duplicateBindErrors :: Env -> BareBind -> [UserError]
duplicateBindErrors vEnv x =
  condError (memberEnv (bindId x) vEnv) (errDupBind x)

largeNumberErrors :: Integer -> SourceSpan -> [UserError]
largeNumberErrors n l = condError (maxInt < abs n) (errLargeNum l n)

maxInt :: Integer
maxInt = 4611686018427387903

unboundVarErrors :: Env -> Id -> SourceSpan -> [UserError]
unboundVarErrors vEnv x l =
  condError (not (memberEnv x vEnv)) (errUnboundVar l x)

--------------------------------------------------------------------------------
-- | Error Constructors
--------------------------------------------------------------------------------
condError :: Bool -> UserError -> [UserError]
condError True  e = [e]
condError False _ = []

errDupParam x =
  mkError (printf "Duplicate parameter '%s'" (bindId x)) (sourceSpan x)
errDupBind x = mkError (printf "Shadow binding '%s'" (bindId x)) (sourceSpan x)
errLargeNum l n = mkError (printf "Number '%d' is too large" n) l
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l
errUnboundFun l f = mkError (printf "Function '%s' is not defined" f) l
