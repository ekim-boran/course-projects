module Language.Diamondback.Asm
  ( asm
  ) where

-- import           Data.Monoid
import qualified Data.List                     as L
import           Language.Diamondback.Types
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
-- | Convert a sequence of x86 `Instructions` into the output assembly
--------------------------------------------------------------------------------
asm :: [Instruction] -> Text
--------------------------------------------------------------------------------
asm instrs = header <> instrsAsm (instrs ++ postlude) <> "\n"

instrsAsm :: [Instruction] -> Text
instrsAsm = L.intercalate "\n" . map instrAsm

header :: Text
header = unlines
  [ "section .text"
  , "extern error"
  , "extern print"
  , "global our_code_starts_here"
  , "our_code_starts_here:"
  ]

-- | FILL: insert instructions containing labels and code
--   for jumping to and calling `error` here.

postlude :: [Instruction]
postlude = concat
  [ dynError (TypeError TNumber)
  , dynError (TypeError TBoolean)
  , dynError ArithOverflow
  ]

errnum (TypeError TNumber ) = 0
errnum (TypeError TBoolean) = 1
errnum (ArithOverflow     ) = 2

dynError :: DynError -> [Instruction]
dynError e =
  [ ILabel (DynamicErr e)
  , IMov (Reg RDI) (Const (errnum e))
  , ICall (Builtin "error")
  ]
--------------------------------------------------------------------------------
instrAsm :: Instruction -> Text
--------------------------------------------------------------------------------
instrAsm (IMov dst val) = printf "  mov %s, %s" (argAsm dst) (argAsm val)
instrAsm (IAdd dst val) = printf "  add %s, %s" (argAsm dst) (argAsm val)
instrAsm (ISub dst val) = printf "  sub %s, %s" (argAsm dst) (argAsm val)
instrAsm (IMul dst val) = printf "  imul %s, %s" (argAsm dst) (argAsm val)
instrAsm (IAnd dst msk) = printf "  and %s, %s" (argAsm dst) (argAsm msk)
instrAsm (IOr  dst msk) = printf "  or  %s, %s" (argAsm dst) (argAsm msk)
instrAsm (IXor dst msk) = printf "  xor %s, %s" (argAsm dst) (argAsm msk)
instrAsm (IShl dst val) = printf "  shl %s, %s" (argAsm dst) (argAsm val)
instrAsm (IShr dst val) = printf "  shr %s, %s" (argAsm dst) (argAsm val)
instrAsm (ISar dst val) = printf "  sar %s, %s" (argAsm dst) (argAsm val)
instrAsm (ICmp a1  a2 ) = printf "  cmp %s, %s" (argAsm a1) (argAsm a2)
instrAsm (IPush  a    ) = printf "  push %s" (argAsm a)
instrAsm (IPop   a    ) = printf "  pop  %s" (argAsm a)
instrAsm (ICall  l    ) = printf "  call %s" (labelAsm l)
instrAsm (ILabel l    ) = printf "%s: " (labelAsm l)
instrAsm (IJe    l    ) = printf "  je  near %s" (labelAsm l)
instrAsm (IJne   l    ) = printf "  jne near %s" (labelAsm l)
instrAsm (IJg    l    ) = printf "  jg  near %s" (labelAsm l)
instrAsm (IJl    l    ) = printf "  jl  near %s" (labelAsm l)
instrAsm (IJo    l    ) = printf "  jo  near %s" (labelAsm l)
instrAsm (IJmp   l    ) = printf "  jmp near %s" (labelAsm l)
instrAsm IRet           = "  ret"

regAsm :: Reg -> Text
regAsm RAX = "rax"
regAsm RBX = "rbx"
regAsm RSP = "rsp"
regAsm RBP = "rbp"
regAsm RSI = "rsi"
regAsm RDI = "rdi"
regAsm RDX = "rdx"
regAsm RCX = "rcx"
regAsm R8  = "r8"
regAsm R9  = "r9"

sizeAsm :: Size -> Text
sizeAsm DWordPtr = "DWORD"
sizeAsm WordPtr  = "WORD"
sizeAsm BytePtr  = "BYTE"

argAsm :: Arg -> Text
argAsm (Const    n) = printf "%d" n
argAsm (HexConst n) = printf "0x%X" n
argAsm (Reg      r) = regAsm r
argAsm (RegOffset n r)
  | 0 <= n    = printf "[%s - %d]" (regAsm r) (8 * n)
  | otherwise = printf "[%s + %d]" (regAsm r) (8 * negate n)
argAsm (RegIndex r i) = printf "[%s + %s * 4]" (regAsm r) (regAsm i)
argAsm (Sized    s a) = printf "%s %s" (sizeAsm s) (argAsm a)

labelAsm :: Label -> Text
labelAsm (BranchTrue i) = printf "label_%d_true" i
labelAsm (BranchDone i) = printf "label_%d_done" i
labelAsm (DefFun     f) = printf "label_def_%s_start" f
labelAsm (DefFunBody f) = printf "label_def_%s_body" f
labelAsm (DynamicErr e) = dynErrorLabel e
labelAsm (Builtin    f) = f

dynErrorLabel :: DynError -> Text
dynErrorLabel (TypeError t) = errorLabel ("non_" <> pprint t)
dynErrorLabel ArithOverflow = errorLabel "overflow"

errorLabel :: Text -> Text
errorLabel l = "internal_error_" <> l -- errorFun l
