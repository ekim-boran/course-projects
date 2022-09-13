module Language.Adder.Asm
  ( asm
  ) where

import qualified Data.List                     as L
import           Language.Adder.Types
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
instrAsm IRet           = "  ret"

regAsm :: Reg -> Text
regAsm EAX = "eax"
regAsm RBP = "rbp"

argAsm :: Arg -> Text
argAsm (Const n) = printf "%d" n
argAsm (Reg   r) = regAsm r
argAsm (RegOffset n r) | 0 <= n    = printf "[%s-%d]" (regAsm r) (n * 4)
                       | otherwise = printf "[%s+%d]" (regAsm r) (n * 4)

