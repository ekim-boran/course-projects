{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Hare.Hare where

import Control.Applicative
import Control.Monad
import Hare.HareMonad

data RE :: * -> * where
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a, b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b

match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = empty
match (Char xs) = do
  x <- readCharacter
  guard (x `elem` xs)
  pure x
match (Seq a b) = do
  ra <- match a
  rb <- match b
  pure (ra, rb)
match (Choose a b) = match a <|> match b -- pure ()
match (Star x) = many (match x)
match (Action f r) = fmap f (match r)

matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a
(=~) = flip (hare . matchAnywhere)

infixr 9 `cons`

cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action (uncurry (:)) (x `Seq` xs)

string :: String -> RE String
string = foldr (\x -> cons (Char [x])) (Action (const []) Empty)

rpt :: Int -> RE a -> RE [a]
rpt n re = foldr (\x -> cons re) (Action (const []) Empty) [1 .. n]

rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x, y) re = choose $ (\x -> rpt x re) <$> (reverse [x .. y])

option :: RE a -> RE (Maybe a)
option re = Action Just re `Choose` Action (const Nothing) Empty

plus :: RE a -> RE [a]
plus re = cons re (Star re)

choose :: [RE a] -> RE a
choose = foldr Choose Fail
