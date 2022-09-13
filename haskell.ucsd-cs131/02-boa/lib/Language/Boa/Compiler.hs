{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Boa.Compiler
  ( compiler
  , compile
  ) where

import           Control.Arrow                  ( (>>>) )
import           Control.Monad                  ( void )
import           Data.Maybe
import           Language.Boa.Asm               ( asm )
import           Language.Boa.Normalizer        ( anormal )
import           Language.Boa.Parser
import           Language.Boa.Types      hiding ( Tag )
import           Prelude                 hiding ( compare )
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> anormal >>> tag >>> compile >>> asm

-- | to test your compiler with code that is ALREADY in ANF comment out
--   the above definition and instead use the below:

-- compiler f = parse f >>> tag >>> compile >>> asm


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag = (SourceSpan, Int)
type AExp = AnfExpr Tag
type IExp = ImmExpr Tag
type ABind = Bind Tag

instance Located Tag where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: Bare -> AExp
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
-- | @compile@ a (tagged-ANF) expr into assembly
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = header ++ compileEnv emptyEnv e ++ footer

header :: [Instruction]
header = [IPush (Reg RBP), IMov (Reg RBP) (Reg RSP)]

footer :: [Instruction]
footer = [IPop (Reg RBP), IRet]

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@(Number{}) = [compileImm env v]

compileEnv env v@(Id{}    ) = [compileImm env v]

compileEnv env e@(Let x e1 e2 l) =
  let instr1      = compileEnv env e1
      (loc, env2) = pushEnv x env
      instr2      = compileEnv env2 e2
  in  instr1 ++ [IMov (stackVar loc) (Reg EAX)] ++ instr2


compileEnv env (Prim1 o v l    ) = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 (_, i)) =
  let instr1 = compileEnv env v
      instrf = compileEnv env e2 ++ [IJmp (BranchDone i)]
      instrt = ILabel (BranchTrue i) : compileEnv env e1
  in  instr1
        <> [ICmp (Reg EAX) (Const 0), IJne (BranchTrue i)]
        <> instrf
        <> instrt
        <> [ILabel (BranchDone i)]




compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)

immArg :: Env -> IExp -> Arg
immArg _   (  Number n _) = repr n
immArg env e@(Id     x _) = case lookupEnv x env of
  Nothing  -> err
  (Just x) -> stackVar x
  where err = abort (errUnboundVar (sourceSpan e) x)
immArg _ e = panic msg (sourceSpan e)
  where msg = "Unexpected non-immExpr in immArg: " ++ show (void e)

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l


--------------------------------------------------------------------------------
-- | Compiling Primitive Operations
--------------------------------------------------------------------------------
compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1 v = arith env IAdd v (Number 1 l)
compilePrim1 l env Sub1 v = arith env ISub v (Number 1 l)

compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 _ env Plus  = arith env IAdd
compilePrim2 _ env Minus = arith env ISub
compilePrim2 _ env Times = arith env IMul

--------------------------------------------------------------------------------
-- | Arithmetic
--------------------------------------------------------------------------------
arith :: Env -> AOp -> IExp -> IExp -> [Instruction]
--------------------------------------------------------------------------------
arith env instr v1 v2 =
  [IMov (Reg EAX) (immArg env v1), instr (Reg EAX) (immArg env v2)]

type AOp = Arg -> Arg -> Instruction


--------------------------------------------------------------------------------
-- | Local Variables
--------------------------------------------------------------------------------
stackVar :: Int -> Arg
--------------------------------------------------------------------------------
stackVar i = RegOffset (-4 * i) RBP

--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Int where
  repr n = Const (fromIntegral n)

instance Repr Integer where
  repr n = Const (fromIntegral n)
