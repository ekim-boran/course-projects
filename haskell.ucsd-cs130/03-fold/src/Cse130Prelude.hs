{-# LANGUAGE NoImplicitPrelude #-}
module Cse130Prelude
  ( module Prelude
  , foldl', foldr
  ) where

import Prelude
  ( Int, String, Bool(..)
  , Show(..), Num(..), Eq(..), Ord(..), Integral(..)
  , error, otherwise
  , ($), (.), id, const
  , length, (++), reverse
  , zip, unzip
  , map
  )

import qualified Data.List (foldl', foldr)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = Data.List.foldl'

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = Data.List.foldr
