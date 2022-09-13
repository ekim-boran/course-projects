{-# LANGUAGE LambdaCase #-}

-- The basic definition of the parsing library as developed in lecture.
-- Operations for building composite parsers are in the module
-- ParserCombinators.

module HW6.Parser
  ( Parser,
    get,
    choose,
    eof,
    filter,
    doParse,
  )
where

import Control.Applicative
import Prelude hiding (filter)

newtype Parser a = P (String -> [(a, String)])

doParse :: Parser a -> String -> [(a, String)]
doParse (P p) = p

get :: Parser Char
get = P (\case (x : xs) -> [(x, xs)]; [] -> [])

eof = P $ \case [] -> [((), [])]; _ : _ -> []

filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> [(x, s') | (x, s') <- doParse p s, f x]

-- | Combine two parsers together in parallel, producing all
-- possible results from either parser.
choose :: Parser a -> Parser a -> Parser a
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

instance Functor Parser where
  fmap = liftA

instance Applicative Parser where
  pure x = P (\cs -> [(x, cs)])
  p1 <*> p2 =
    P
      ( \cs -> do
          (f, cs') <- doParse p1 cs
          (x, cs'') <- doParse p2 cs'
          return (f x, cs'')
      )

instance Alternative Parser where
  -- the parser that always fails
  empty = P $ const []

  p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
    [] -> []
    x : _ -> [x]
