module A05_Defs
  ( ShrMem (..)
  ) where

import           Control.Concurrent.MVar
import           Data.Map                      as Map

import           A02_Defs
import           A03_Defs

data ShrMem = ShrMem (MVar (Map Loc (MVar Val)))
