{-# LANGUAGE TemplateHaskell #-}

import Course1.W1 as W1
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 5000} W1.prop
