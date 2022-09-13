module Language.Boa.Asm
  ( asm
  ) where

import qualified Data.List                     as L
import           Language.Boa.Types
import           Text.Printf                    ( printf )

--------------------------------------------------------------------------------
-- | Convert a sequence of x86 `Instructions` into the output assembly
--------------------------------------------------------------------------------
asm :: [Instruction] -> Text
--------------------------------------------------------------------------------
asm instrs = header <> instrsAsm instrs <> "\n"

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

--------------------------------------------------------------------------------
instrAsm :: Instruction -> Text
--------------------------------------------------------------------------------
instrAsm (IMov dst val) = printf "  mov %s, %s" (argAsm dst) (argAsm val)
instrAsm (IAdd dst val) = printf "  add %s, %s" (argAsm dst) (argAsm val)
instrAsm (ISub dst val) = printf "  sub %s, %s" (argAsm dst) (argAsm val)
instrAsm (IMul dst val) = printf "  imul %s, %s" (argAsm dst) (argAsm val)
instrAsm (ICmp a1  a2 ) = printf "  cmp %s, %s" (argAsm a1) (argAsm a2)
instrAsm (ILabel l    ) = printf "%s:" (labelAsm l)
instrAsm (IJe    l    ) = printf "  je %s" (labelAsm l)
instrAsm (IJne   l    ) = printf "  jne %s" (labelAsm l)
instrAsm (IJmp   l    ) = printf "  jmp %s" (labelAsm l)
instrAsm IRet           = "  ret"
instrAsm (IPush arg)    = printf "  push %s" (argAsm arg)
instrAsm (IPop  arg)    = printf "  pop  %s" (argAsm arg)


regAsm :: Reg -> Text
regAsm EAX = "eax"
regAsm RBP = "rbp"
regAsm RSP = "rsp"

argAsm :: Arg -> Text
argAsm (Const n) = printf "%d" n
argAsm (Reg   r) = regAsm r
argAsm (RegOffset n r) | 0 <= n    = printf "[%s + %d]" (regAsm r) n
                       | otherwise = printf "[%s - %d]" (regAsm r) (negate n)

labelAsm :: Label -> Text
labelAsm (BranchTrue i) = printf "label_%d_true" i
labelAsm (BranchDone i) = printf "label_%d_done" i
