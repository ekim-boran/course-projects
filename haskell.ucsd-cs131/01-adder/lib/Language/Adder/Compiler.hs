{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Adder.Compiler
  ( compiler
  ) where

import           Control.Arrow                  ( (>>>) )
import           Data.Maybe
import           Language.Adder.Asm             ( asm )
import           Language.Adder.Parser          ( parse )
import           Language.Adder.Types
import           Prelude                 hiding ( compare )
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> compile >>> asm

--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag = SourceSpan
type AExp = Expr Tag
type ABind = Bind Tag


--------------------------------------------------------------------------------
-- | @compile@ a (tagged-ANF) expr into assembly
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = compileEnv emptyEnv e ++ [IRet]

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv _ (Number n _) = [IMov (Reg EAX) (repr n)]

compileEnv env (Prim1 Add1 e _) =
  let f = compileEnv env e in f ++ [IAdd (Reg EAX) (Const 1)]

compileEnv env (Prim1 Sub1 e _) =
  let f = compileEnv env e in f ++ [IAdd (Reg EAX) (Const (-1))]

compileEnv env (Id x l) = case lookupEnv x env of
  Nothing          -> error "not bound"
  (Just stack_loc) -> [IMov (Reg EAX) (RegOffset stack_loc RBP)]

compileEnv env (Let x e1 e2 _) =
  let instr1      = compileEnv env e1
      (loc, env2) = pushEnv x env
      instr2      = compileEnv env2 e2
  in  instr1 ++ [IMov (RegOffset loc RBP) (Reg EAX)] ++ instr2

--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Int where
  repr n = Const (fromIntegral n)

instance Repr Integer where
  repr n = Const (fromIntegral n)
