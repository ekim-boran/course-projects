{-# LANGUAGE NoMonomorphismRestriction #-}

module Lectures.SecretCode where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (toUpper)
import System.FilePath
import Test.HUnit

cypher = "thequickbrownfxjmpsvlazydg"

codeMap = code ++ fmap (bimap toUpper toUpper) code
  where
    code = zip ['a' .. 'z'] cypher

-- >>> codeMap

encodeChar codeMap c
  | (Just n) <- lookup c codeMap = n
  | otherwise = c

encodeLine codeMap = fmap (encodeChar codeMap)

encodeContent :: [(Char, Char)] -> String -> String
encodeContent codeMap = unlines . fmap (encodeLine codeMap) . reverse . lines

testEncodeChar =
  runTestTT $
    TestList
      [ encodeChar codeMap 'a' ~?= 't',
        encodeChar codeMap '.' ~?= '.'
      ]

testEncodeLine = runTestTT $ TestList [encodeLine codeMap "abc defgh" ~?= "the quick"]

testEncodeContent =
  runTestTT $
    encodeContent codeMap "abc\n defgh\n" ~?= " quick\nthe\n"

-- >>> testEncodeChar
-- >>> testEncodeLine
-- >>> testEncodeContent
-- Cases: 2  Tried: 0  Errors: 0  Failures: 0
-- Cases: 2  Tried: 1  Errors: 0  Failures: 0
--
-- Cases: 2  Tried: 2  Errors: 0  Failures: 0
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}
-- <BLANKLINE>
-- Cases: 1  Tried: 0  Errors: 0  Failures: 0
--
-- Cases: 1  Tried: 1  Errors: 0  Failures: 0
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
-- <BLANKLINE>
-- Cases: 1  Tried: 0  Errors: 0  Failures: 0
--
-- Cases: 1  Tried: 1  Errors: 0  Failures: 0
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
--
encodeFile :: FilePath -> IO ()
encodeFile f =
  if takeExtension f == "code"
    then putStrLn "Cannot encode .code files"
    else do
      let outFilePath = replaceExtension f "code"
      readFile f >>= (writeFile outFilePath . encodeContent codeMap)

testEncodeFile = encodeFile "README.md"

-- >>>  testEncodeFile
--

foldlviafoldr f acc xs = foldr g id xs acc
  where
    g x h xs = h $ f xs x

-- >>> foldlviafoldr (flip (:)) [] [0..100]
-- [100,99,98,97,96,95,94,93,92,91,90,89,88,87,86,85,84,83,82,81,80,79,78,77,76,75,74,73,72,71,70,69,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0]
--
