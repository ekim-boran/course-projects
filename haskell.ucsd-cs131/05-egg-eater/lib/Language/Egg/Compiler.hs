{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Egg.Compiler
  ( compiler
  , compile
  ) where

import           Control.Arrow                  ( (>>>) )
import           Control.Monad                  ( void )
import           Data.Bits                      ( shift )
import           Data.Bits
import           Data.Maybe
import           Language.Egg.Asm               ( asm )
import           Language.Egg.Checker           ( check
                                                , errUnboundVar
                                                )
import           Language.Egg.Normalizer        ( anormal )
import           Language.Egg.Parser            ( parse )
import           Language.Egg.Types      hiding ( Tag )
import           Prelude                 hiding ( compare )

--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> check >>> anormal >>> tag >>> compile >>> asm

--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Ann = (SourceSpan, Int)
type AExp = AnfExpr Ann
type IExp = ImmExpr Ann
type ABind = Bind Ann
type ADcl = Decl Ann
type APgm = Program Ann

instance Located Ann where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

annTag :: Ann -> Int
annTag = snd


--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: AnfProgram SourceSpan -> APgm
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
compile :: APgm -> [Instruction]
--------------------------------------------------------------------------------
compile (Prog ds e) = top ++ decls where
  env = fromListEnv $ fmap (\d -> (bindId $ fName d, 0)) ds
  decls =
    [ instr | d <- ds, instr <- compileDecl (fName d) (fArgs d) (fBody d) ]
  top = funEntry n ++ compileEnv env e ++ funExit n
  n   = countVars e
-- | @compileDecl f xs body@ returns the instructions of `body` of 
--   a function 'f' with parameters 'xs'
--   wrapped with code that (1) sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.
compileDecl :: Bind a -> [Bind a] -> AExp -> [Instruction]
compileDecl f xs body =
  [ILabel (DefFun (bindId f))]
    ++ funEntry n                            -- 1. setup  stack frame RBP/RSP
    ++ [ILabel (DefFunBody (bindId f))]
    ++ copyArgs xs                           -- 2. copy parameters into stack slots
    ++ compileEnv env body                   -- 3. execute 'body' with result in RAX
    ++ funExit n                             -- 4. teardown stack frame & return 
 where
  env = fromListEnv (zip (bindId <$> xs) [1 ..])
  n   = length xs + countVars body



copyArgs :: [a] -> [Instruction]
copyArgs xs = fmap f $ take (length xs) $ zip [1 ..] regs
  where f (i, reg) = IMov (RegOffset i (RBP)) reg

regs = [Reg RDI, Reg RSI, Reg RDX]

boundary :: Int -> Int
boundary n = 8 * if even n then n else n + 1

-- FILL: insert instructions for setting up stack for `n` local vars
funEntry :: Int -> [Instruction]
funEntry n =
  [ IPush (Reg RBP)
  , IMov (Reg RBP) (Reg RSP)
  , ISub (Reg RSP) (Const (boundary n))
  ]

-- FILL: clean up stack & labels for jumping to error
funExit :: Int -> [Instruction]
funExit n = [IAdd (Reg RSP) (Const (boundary n)), IPop (Reg RBP), IRet]



--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
--------------------------------------------------------------------------------
countVars (Let _ e  b  _) = max (countVars e) (1 + countVars b)
countVars (If  v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _               = 0

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@(Number{} ) = [compileImm env v]

compileEnv env v@(Boolean{}) = [compileImm env v]

compileEnv env v@(Id{}     ) = [compileImm env v]

compileEnv env e@(Let{}    ) = is ++ compileEnv env' body
 where
  (env' , is  ) = compileBinds env [] binds
  (binds, body) = exprBinds e

compileEnv env (Prim1 o v l    ) = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If    v e1 e2 l) = compileIf l env v e1 e2

compileEnv env (App f vs l     ) = compileApp l env f vs

compileEnv env (Tuple es l     ) = compileTuple l env es

compileEnv env (GetItem vE vI _) =
  compileEnv env vI
    ++ [IMov (Reg R14) (Reg RAX)]
    ++ [ISar (Reg R14) (Const 1)]
    ++ [IAdd (Reg R14) (Const 1)]
    ++ [compileImm env vE]
    ++ [IAnd (Reg RAX) complemented]
    ++ [IMov (Reg RAX) (RegIndex RAX R14)]


compileTuple l env es =
  [IMov (Reg RAX) (repr (length es))]
    ++ [IMov (RegOffset 0 R15) (Reg RAX)]
    ++ [ ins
       | (i, e) <- zip [1 ..] es
       , ins <- [compileImm env e, IMov (RegOffset (-i) R15) (Reg RAX)]
       ]
    ++ [IMov (Reg RAX) (Reg R15)]
    ++ [IOr (Reg RAX) (typeTag TTuple)]
    ++ [IAdd (Reg R15) (Const $ (1 + length es) * 8)]



compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg RAX) (immArg env v)

compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []       = (env, is)
compileBinds env is (b : bs) = compileBinds env' (is ++ is') bs
  where (env', is') = compileBind env b

compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
 where
  is        = compileEnv env e ++ [IMov (stackVar i) (Reg RAX)]
  (i, env') = pushEnv x env


compileApp l env f vs =
  [ i | (v, reg) <- zip vs regs, i <- [compileImm env v, IMov reg (Reg RAX)] ]
  ++ [ICall (DefFun f)]

checkTy :: Env -> Ty -> IExp -> [Instruction]
checkTy env ty v =
  [ IMov (Reg RBX) (immArg env v)
  , IAnd (Reg RBX) (typeMask ty)
  , ICmp (Reg RBX) (typeTag ty)
  , IJne (DynamicErr (TypeError ty))
  ]

isTy :: Ann -> Env -> Ty -> IExp -> [Instruction]
isTy (_, l) env ty v =
  [ IMov (Reg RBX) (immArg env v)
    , IAnd (Reg RBX) (typeMask ty)
    , ICmp (Reg RBX) (typeTag ty)
    , IJe (BranchTrue l)
    ]
    ++ [IMov (Reg RAX) (repr False)]
    ++ [IJmp (BranchDone l)]
    ++ [ILabel (BranchTrue l)]
    ++ [IMov (Reg RAX) (repr True)]
    ++ [ILabel (BranchDone l)]

--------------------------------------------------------------------------------
ops :: Env -> (Arg -> Arg -> Instruction) -> IExp -> IExp -> [Instruction]
ops env instr v1 v2 =
  checkTy env TNumber v1
    ++ checkTy env TNumber v2
    ++ [IMov (Reg RAX) (immArg env v1)]
    ++ [ISar (Reg RAX) (Const 1)]
    ++ [IMov (Reg RBX) (immArg env v2)]
    ++ [ISar (Reg RBX) (Const 1)]
    ++ [instr (Reg RAX) (Reg RBX)]
    ++ [IJo (DynamicErr ArithOverflow)] -- wierd workaround
    ++ [IShl (Reg RAX) (Const 1)]
    ++ [IJo (DynamicErr ArithOverflow)] -- wierd workaround

compareOp :: Ann -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compareOp (_, l) env o v1 v2 = tyCheck ++ rest where
  tyCheck = case o of
    Equal -> []
    _     -> checkTy env TNumber v1 ++ checkTy env TNumber v2
  rest =
    [IMov (Reg RAX) (immArg env v1)]
      ++ [ICmp (Reg RAX) (immArg env v2)]
      ++ [jmpinst o (BranchTrue l)]
      ++ [IMov (Reg RAX) (repr False)]
      ++ [IJmp (BranchDone l)]
      ++ [ILabel (BranchTrue l)]
      ++ [IMov (Reg RAX) (repr True)]
      ++ [ILabel (BranchDone l)]
  jmpinst Less    = IJl
  jmpinst Greater = IJg
  jmpinst Equal   = IJe
  jmpinst _       = undefined

compilePrim1 :: Ann -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1    v = ops env IAdd v (Number 1 l)
compilePrim1 l env Sub1    v = ops env ISub v (Number 1 l)
compilePrim1 l env IsNum   v = isTy l env TNumber v
compilePrim1 l env IsBool  v = isTy l env TBoolean v
compilePrim1 l env IsTuple v = isTy l env TTuple v

compilePrim1 _ env Print v =
  compileImm env v : [IMov (Reg RDI) (Reg RAX), ICall (Builtin "print")]

compilePrim2 :: Ann -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 _ env Plus  a b = ops env IAdd a b
compilePrim2 _ env Minus a b = ops env ISub a b
compilePrim2 _ env Times a b = ops env IMul a b
compilePrim2 l env o     a b = compareOp l env o a b

compileIf :: Ann -> Env -> Expr Ann -> Expr Ann -> Expr Ann -> [Instruction]
compileIf (_, l) env v e1 e2 =
  let instr1 = checkTy env TBoolean v ++ compileEnv env v
      instrf = compileEnv env e2 ++ [IJmp (BranchDone l)]
      instrt = ILabel (BranchTrue l) : compileEnv env e1
  in  instr1
        <> [IShr (Reg RAX) (Const 31), IJne (BranchTrue l)]
        <> instrf
        <> instrt
        <> [ILabel (BranchDone l)]



immArg :: Env -> IExp -> Arg
immArg _   (  Number  n _) = repr n
immArg _   (  Boolean b _) = repr b
immArg env e@(Id      x _) = stackVar (fromMaybe err (lookupEnv x env))
  where err = abort (errUnboundVar (sourceSpan e) x)
immArg _ e = panic msg (sourceSpan e)
  where msg = "Unexpected non-immExpr in immArg: " ++ show (void e)

stackVar :: Int -> Arg
stackVar i = RegOffset i RBP


--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Bool where
  repr True  = HexConst 0xffffffff
  repr False = HexConst 0x7fffffff

instance Repr Int where
  repr n = Const (fromIntegral (shift n 1))

instance Repr Integer where
  repr n = Const (fromIntegral (shift n 1))

typeTag :: Ty -> Arg
typeTag TNumber  = HexConst 0x00000000
typeTag TBoolean = HexConst 0x7fffffff
typeTag TTuple   = HexConst 0x00000001

complemented = HexConst (complement 0x00000001)

typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
typeMask TTuple   = HexConst 0x00000007
