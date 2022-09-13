-- Advanced Programming, HW 2
-- CIS 552,  University of Pennsylvania

-- DO NOT MODIFY THIS FILE

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module HW2.XMLTypes where

-- | A simplified datatype for storing XML
data SimpleXML
  = PCDATA String
  | Element ElementName [SimpleXML]
  deriving (Show)

type ElementName = String

-- | Convert a SimpleXML value to a string
xml2string :: SimpleXML -> String
xml2string (PCDATA s) = s
xml2string (Element tag []) =
  "<" ++ tag ++ "/>"
xml2string (Element tag body) =
  "<" ++ tag ++ ">" ++ concatMap xml2string body ++ "</" ++ tag ++ ">"
