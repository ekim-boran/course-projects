{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.FDL.Compiler
  ( compiler
  , compile
  ) where

import           Control.Arrow                  ( (>>>) )
import           Control.Monad                  ( void )
import           Data.Bits                      ( shift )
import           Data.Bits
import           Data.Maybe
import qualified Data.Set                      as S
import           Debug.Trace
import           Language.FDL.Asm               ( asm )
import           Language.FDL.Checker           ( check
                                                , errUnboundVar
                                                )
import           Language.FDL.Normalizer        ( anormal )
import           Language.FDL.Parser            ( parse )
import           Language.FDL.Types      hiding ( Tag )
import qualified Language.FDL.Utils            as U
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

instance Located Ann where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

annTag :: Ann -> Int
annTag = snd


--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: AnfExpr SourceSpan -> AExp
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = trace "" $ compileDecl Nothing [] [] e
--(show $ fmap (const ()) e)
{- | @compileDecl f xs body@ returns the instructions of `body` 
     of a function with optional name 'f', parameters 'xs', and 
     free variables 'ys'. The instructions should:

     1. setup  stack frame RBP/RSP
     2. copy parameters (following x86-64 call convention) to stack slots
     3. copy (closure) free vars (from tuple at RDI) to stack slots
     4. execute 'body' with result in RAX
     5. teardown stack frame & return 

     @name@ is the (optional) name of the function, that may be useful to implement recursion.
-}
--compileDecl :: Maybe ABind -> [Id] -> [ABind] -> AExp -> [Instruction]
--compileDecl f frees params body = error "fill this in"

compileDecl :: Maybe ABind -> [Id] -> [ABind] -> AExp -> [Instruction]
compileDecl nm frees params body =
  [ILabel (DefFun nm' (annTag $ getLabel body))]
    ++ funEntry n
    ++ [ILabel (DefFunBody nm' (annTag $ getLabel body))]
    ++ copyArgs params
    ++ restore (length params + 1) frees
    ++ compileEnv env body
    ++ funExit n
 where
  env = fromListEnv (zip ((bindId <$> params) ++ [nm'] ++ (frees)) [1 ..])
  n   = length frees + length params + countVars body + 1
  nm' = declId nm

declId :: Maybe (Bind a) -> Id
declId Nothing  = "$self"
declId (Just f) = bindId f

-- | @compileArgs xs@ returns the instructions needed to copy the parameter values
--   FROM the combination of `rdi`, `rsi`, ... INTO this function's "frame".
--   RDI -> [RBP - 8], RSI -> [RBP - 16] 
copyArgs :: [a] -> [Instruction]
copyArgs xs = fmap f $ take (length xs) $ zip [1 ..] regs
  where f (i, reg) = IMov (RegOffset i (RBP)) reg

regs = [Reg RSI, Reg RDX, Reg RCX, Reg R8, Reg R9]

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




-- | @restore ys@ uses the closure passed in at RDI to copy the 
--  free-vars 'ys' onto the stack starting at 'base' (after the params)
restore :: Int -> [Id] -> [Instruction]
restore base ys =
  IMov (RegOffset base RBP) (Reg RDI)
    : [ instr
      | (i, j, y) <- zip3 [(base + 1) ..] [2 ..] ys
      , instr     <-
        [IMov (Reg R14) (RegOffset (-j) RDI), IMov (RegOffset i RBP) (Reg R14)]
      ]

lambda :: Ann -> Env -> Maybe ABind -> [ABind] -> AExp -> [Instruction]
lambda l env f xs e =
  IJmp end
    :  ILabel start
    :  compileDecl f ys xs e
    ++ ILabel end
    :  lambdaClosure l env arity start ys
 where
  ys           = freeVars (fun f xs e l)
  arity        = length xs
  (start, end) = startEnd (snd l) f

startEnd :: Int -> Maybe ABind -> (Label, Label)
startEnd i Nothing           = (LamStart i, LamEnd i)
startEnd i (Just (Bind f _)) = (DefFun f i, DefEnd f i)

{- | @lambdaClosure l env arity ys@ returns a sequence of instructions,
     that have the effect of writing into RAX the value of the closure
     corresponding to the lambda-functiion l. To do so we must:
  
     1. allocate space on the tuple for a "tuple" of
          (arity, LamStart l, y1, y2, ... , yn) 

     2. write the values of arity, y1...yn into the above

     3. adjust the address bits to ensure 101-ending
-}


lambdaClosure :: Ann -> Env -> Int -> Label -> [Id] -> [Instruction]
lambdaClosure l env arity start freeVars =
  mv (IMov (Reg RAX) (repr arity)) 0
    ++ mv (IMov (Reg RAX) (CodePtr start)) 1
    ++ concat
         [ mv
             (IMov (Reg RAX)
                   (stackVar (fromMaybe (error "boran") (lookupEnv e env)))
             )
             i
         | (i, e) <- zip [2 ..] freeVars
         ]
    ++ heapFinish TClosure (length freeVars + 2)



--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
--------------------------------------------------------------------------------
countVars (Let _ e  b  _) = max (countVars e) (1 + countVars b)
countVars (If  v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _               = 0


freeVars :: Expr a -> [Id]
freeVars = S.toList . S.fromList . freeVarsEnv emptyEnv


freeVarsEnv :: Env -> Expr a -> [Id]
freeVarsEnv env v@Number{}  = []
freeVarsEnv env v@Boolean{} = []
freeVarsEnv env (Id id a)   = case lookupEnv id env of
  Nothing  -> [id]
  (Just _) -> []
freeVarsEnv env (Let a e1 e2 l) =
  freeVarsEnv env e1 ++ freeVarsEnv (addEnv a env) e2
freeVarsEnv env (Prim1 o v l    ) = freeVarsEnv env v
freeVarsEnv env (Prim2 o v1 v2 l) = freeVarsEnvs env [v1, v2]
freeVarsEnv env (If    v e1 e2 l) = freeVarsEnvs env [v, e1, e2]
freeVarsEnv env (Tuple es l     ) = freeVarsEnvs env es
freeVarsEnv env (GetItem vE vI l) = freeVarsEnvs env [vE, vI]
freeVarsEnv env (Lam xs e l) = freeVarsEnvs (foldl (flip addEnv) env xs) [e]
freeVarsEnv env (Fun f xs e l) =
  freeVarsEnvs (foldl (flip addEnv) env (f : xs)) [e]
freeVarsEnv env (App v vs l) = freeVarsEnvs env (v : vs)

freeVarsEnvs :: Env -> [Expr a] -> [Id]
freeVarsEnvs env = foldl (\xs x' -> freeVarsEnv env x' ++ xs) []


--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@Number{}  = [compileImm env v]

compileEnv env v@Boolean{} = [compileImm env v]

compileEnv env v@Id{}      = [compileImm env v]

compileEnv env e@Let{}     = is ++ compileEnv env' body
 where
  (env' , is  ) = compileBinds env [] binds
  (binds, body) = exprBinds e

compileEnv env (Prim1 o v l    ) = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If    v e1 e2 l) = compileIf l env v e1 e2

compileEnv env (Tuple es l     ) = compileTuple l env es
compileEnv env (GetItem vE vI l) = compileGetItem l env vE vI

compileEnv env (Lam     xs e  l) = lambda l env Nothing xs e
compileEnv env (Fun f xs e l   ) = lambda l env (Just f) xs e
compileEnv env (App v vs l     ) = compileApp l env v vs



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



compileGetItem l env vE vI =
  compileEnv env vI
    ++ [IMov (Reg R14) (Reg RAX)]
    ++ [ISar (Reg R14) (Const 1)]
    ++ [IAdd (Reg R14) (Const 1)]
    ++ [compileImm env vE]
    ++ [IAnd (Reg RAX) (complementTy TTuple)]
    ++ [IMov (Reg RAX) (RegIndex RAX R14)]



mv instr count = [instr, IMov (RegOffset (-count) R15) (Reg RAX)]
heapFinish ty len =
  [ IMov (Reg RAX) (Reg R15)
  , IOr (Reg RAX) (typeTag ty)
  , IAdd (Reg R15) (Const $ len * 8)
  ]

compileTuple l env es =
  mv (IMov (Reg RAX) (repr (length es))) 0
    ++ concat [ mv (compileImm env e) i | (i, e) <- zip [1 ..] es ]
    ++ heapFinish TTuple (length es + 1)


compileApp l env f vs =
  compileEnv env f
    ++ [IAnd (Reg RAX) (complementTy TClosure)]
    ++ [IMov (Reg RDI) (Reg RAX)]
    ++ [IMov (Reg R14) (RegOffset (0) RDI)]   --- length field
    ++ [ICmp (Reg R14) (repr (length vs))]
    ++ [IJne (DynamicErr ArityError)]
    ++ [ i
       | (v, reg) <- zip vs regs
       , i        <- [compileImm env v, IMov reg (Reg RAX)]
       ]
    ++ [ICall (RegOffset (-1) RDI)]

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
  compileImm env v
    : [IMov (Reg RDI) (Reg RAX), ICall (CodePtr $ Builtin "print")]

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
typeTag TClosure = HexConst 0x00000005


complementTy :: Ty -> Arg
complementTy ty = let (HexConst x) = typeTag ty in HexConst (complement x)


typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
typeMask TTuple   = HexConst 0x00000007
typeMask TClosure = HexConst 0x00000007
