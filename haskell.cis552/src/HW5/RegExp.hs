{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HW5.RegExp where

import Control.Applicative (Alternative (..))
import Control.Monad (ap, liftM2)
import Data.Bifunctor (first)
import Data.Foldable
import qualified Data.IntMap as M
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, member, null, singleton, size, toList)
import Debug.Trace
import qualified GHC.Float as Set
import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.QuickCheck.Function
import Prelude hiding (either)

main :: IO ()
main = return ()

data RegExp
  = Char (Set Char) -- single literal character
  | Alt RegExp RegExp -- r1 | r2   (alternation)
  | Seq RegExp RegExp -- r1 r2     (concatenation)
  | Star RegExp -- r*        (Kleene star)
  | Empty -- ε, accepts empty string
  | Void -- ∅, always fails
  | Mark String RegExp -- (for marked subexpressions, see (b) below)
  deriving (Show, Eq)

char :: Char -> RegExp
char = Char . Set.singleton

chars :: String -> RegExp
chars = Char . Set.fromList

lower, upper, letter, digit, punc, white, anyc, anyc' :: RegExp
lower = chars ['a' .. 'z']
upper = chars ['A' .. 'Z']
digit = chars ['0' .. '9']
punc = chars "<>!/.*()?@"
white = chars " \n\r\t"
anyc' = lower `Alt` upper `Alt` digit `Alt` punc `Alt` white
anyc =
  chars $
    ['a' .. 'z']
      ++ ['A' .. 'Z']
      ++ ['0' .. '9']
      ++ "<>!/.*()?@"
      ++ "\n\r\t"
letter = chars $ ['A' .. 'Z'] ++ ['a' .. 'z']

word :: String -> RegExp
word = foldr (Seq . char) Empty

cis552 :: RegExp
cis552 = word "cis552"

boldHtml :: RegExp
boldHtml = word "<b>" `Seq` Star anyc `Seq` word "</b>"

plus :: RegExp -> RegExp
plus pat = pat `Seq` Star pat

-- (a)

-- all decompositions of a string into two different pieces
--  >>>   split "abc"
--  [("","abc"),("a","bc"),("ab","c"),("abc","")]
--

-- == [("","abc"),("a","bc"),("ab","c"),("abc","")]
split :: [a] -> [([a], [a])]
split [] = [([], [])]
split (x : xs) = ([], x : xs) : fmap (first (x :)) (split xs)

-- >>> parts "abc"
-- [["abc"],["ab","c"],["a","bc"],["a","b","c"]]
--

-- all decompositions of a string into multi-part (nonempty) pieces
-- parts "abc" = [["abc"],["a","bc"], ["ab","c"], ["a","b","c"]]
parts :: [a] -> [[[a]]]
parts [a] = [[[a]]]
parts a@(x : xs) = fmap (\(a : as) -> (x : a) : as) (parts xs) ++ fmap ([x] :) (parts xs)

accept :: RegExp -> String -> Bool
accept (Mark _ r) s = accept r s
accept (Char set) [x] = Set.member x set
accept (Char set) _ = False
accept (Alt r1 r2) s = accept r1 s || accept r2 s
accept (Seq r1 r2) s = or [accept r1 l && accept r2 r | (l, r) <- split s]
accept (Star r) [] = True
accept (Star r) s = or [and [accept r x | x <- xs] | xs <- parts s]
accept Empty s = null s
accept Void s = False

testAccept :: Test
testAccept =
  TestList
    [ not (accept Void "a") ~? "nothing is void",
      not (accept Void "") ~? "really, nothing is void",
      accept Empty "" ~? "accept Empty true",
      not (accept Empty "a") ~? "not accept Empty",
      accept lower "a" ~? "accept lower",
      not (accept lower "A") ~? "not accept lower",
      accept boldHtml "<b>cis552</b>!</b>" ~? "cis552!",
      not (accept boldHtml "<b>cis552</b>!</b") ~? "no trailing"
    ]

x = runTestTT testAccept

-- >>> x
-- Cases: 8  Tried: 0  Errors: 0  Failures: 0
-- Cases: 8  Tried: 1  Errors: 0  Failures: 0
-- Cases: 8  Tried: 2  Errors: 0  Failures: 0
-- Cases: 8  Tried: 3  Errors: 0  Failures: 0
-- Cases: 8  Tried: 4  Errors: 0  Failures: 0
-- Cases: 8  Tried: 5  Errors: 0  Failures: 0
-- Cases: 8  Tried: 6  Errors: 0  Failures: 0
-- Cases: 8  Tried: 7  Errors: 0  Failures: 0
--
-- Cases: 8  Tried: 8  Errors: 0  Failures: 0
-- Counts {cases = 8, tried = 8, errors = 0, failures = 0}
--

boldHtmlPat :: RegExp
boldHtmlPat = word "<b>" `Seq` Mark "<b>" (Star anyc) `Seq` word "</b>"

namePat :: RegExp
namePat = Mark "first" (plus letter) `Seq` Star white `Seq` Mark "last" (plus letter)

wordsPat :: RegExp
wordsPat = Star (Mark "word" (plus lower) `Seq` Star white)

testPat :: Test
testPat =
  TestList
    [ patAccept boldHtmlPat "<b>cis552" ~?= Nothing,
      patAccept boldHtmlPat "<b>cis552!</b>"
        ~?= Just (Map.fromList [("<b>", ["cis552!"])]),
      patAccept boldHtmlPat "<b>cis552</b>!</b>"
        ~?= Just (Map.fromList [("<b>", ["cis552</b>!"])]),
      patAccept namePat "Haskell  Curry"
        ~?= Just (Map.fromList [("first", ["Haskell"]), ("last", ["Curry"])]),
      patAccept wordsPat "a    b c   d e"
        ~?= Just (Map.fromList [("word", ["a", "b", "c", "d", "e"])])
    ]

type Match = Map String [String]

headMaybe (x : xs) = Just x
headMaybe [] = Nothing

patAccept :: RegExp -> String -> Maybe Match
patAccept (Mark a r) s = Map.insertWith (flip (++)) a [s] <$> patAccept r s
patAccept (Char set) [x] | Set.member x set = Just Map.empty
patAccept (Char set) _ = Nothing
patAccept (Alt r1 r2) s = patAccept r1 s <|> patAccept r2 s
patAccept (Seq r1 r2) s = asum [Map.unionWith (++) <$> patAccept r1 l <*> patAccept r2 r | (l, r) <- split s]
patAccept (Star r) [] = Just Map.empty
patAccept (Star r) s = asum [foldl (Map.unionWith (++)) Map.empty <$> sequence [patAccept r x | x <- xs] | xs <- parts s]
patAccept Empty [] = Just Map.empty
patAccept Empty _ = Nothing
patAccept Void s = Nothing

-- (c)
testMatch :: Test
testMatch =
  TestList
    [ accept boldHtml "<b>cis552</b>!</b>" ~? "cis552!",
      not (match boldHtml "<b>cis552</b>!</b") ~? "no trailing",
      not (match Void "a") ~? "nothing is void",
      not (match Void "") ~? "really, nothing is void",
      match Empty "" ~? "accept Empty true",
      not (match Empty "a") ~? "not accept Empty",
      match lower "a" ~? "accept lower",
      not (match lower "A") ~? "not accept lower",
      not (match boldHtml "<b>cis552</b>!</b") ~? "no trailing",
      not (match boldHtmlPat "<b>cis552") ~? "no trailing",
      match boldHtmlPat "<b>cis552!</b>"
        ~? "no trailing",
      match boldHtmlPat "<b>cis552</b>!</b>"
        ~? "no trailing",
      match namePat "Haskell  Curry"
        ~? "no trailing",
      match wordsPat "a    b c   d e"
        ~? "no trailing"
    ]

-- >>> runTestTT testMatch
-- Cases: 14  Tried: 0  Errors: 0  Failures: 0
-- Cases: 14  Tried: 1  Errors: 0  Failures: 0
-- Cases: 14  Tried: 2  Errors: 0  Failures: 0
-- Cases: 14  Tried: 3  Errors: 0  Failures: 0
-- Cases: 14  Tried: 4  Errors: 0  Failures: 0
-- Cases: 14  Tried: 5  Errors: 0  Failures: 0
-- Cases: 14  Tried: 6  Errors: 0  Failures: 0
-- Cases: 14  Tried: 7  Errors: 0  Failures: 0
-- Cases: 14  Tried: 8  Errors: 0  Failures: 0
-- Cases: 14  Tried: 9  Errors: 0  Failures: 0
-- Cases: 14  Tried: 10  Errors: 0  Failures: 0
-- Cases: 14  Tried: 11  Errors: 0  Failures: 0
-- Cases: 14  Tried: 12  Errors: 0  Failures: 0
-- Cases: 14  Tried: 13  Errors: 0  Failures: 0
--
-- Cases: 14  Tried: 14  Errors: 0  Failures: 0
-- Counts {cases = 14, tried = 14, errors = 0, failures = 0}
--

match :: RegExp -> String -> Bool
match r s = nullable (foldl deriv r s)

-- | `nullable r` return `True` when `r` matches the empty string
nullable :: RegExp -> Bool
nullable Empty = True
nullable (Star r) = True
nullable (Seq r1 r2) = nullable r1 && nullable r2
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Mark _ r) = nullable r
nullable _ = False

-- |  Takes a regular expression `r` and a character `c`,
-- and computes a new regular expression that accepts word `w` if `cw` is
-- accepted by `r`.
deriv :: RegExp -> Char -> RegExp
deriv Empty c = Void
deriv Void c = Void
deriv (Char set) c | Set.member c set = Empty
deriv (Char set) c = Void
deriv (Alt r1 r2) c = deriv r1 c `Alt` deriv r2 c
deriv (Mark _ r) c = deriv r c
deriv (Star r) c = deriv r c `Seq` Star r
deriv (Seq r1 r2) c = if nullable r1 then deriv r1 c `Seq` r2 `Alt` deriv r2 c else deriv r1 c `Seq` r2

xa = match (Empty `Seq` Star (Char $ Set.singleton 'a') `Seq` Star (Char $ Set.singleton 'b')) "bbb"

-- >>>xa
-- True
--

-- (d)

rStar :: RegExp -> RegExp
rStar (Star x) = Star x -- two iterations is the same as one
rStar Empty = Empty -- iterating the empty string is the empty string
rStar Void = Empty -- zero or more occurrences of void is empty
rStar r = Star r -- no optimization

composeAp :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
composeAp f x = (fmap (<*>)) (f) <*> x

genRegExpString :: RegExp -> Maybe (Gen String)
genRegExpString Void = Nothing
genRegExpString Empty = Just (return "")
genRegExpString (Char s) = if Set.null s then Nothing else Just (pure <$> elements (Set.toList s))
genRegExpString (Seq r1 r2) = (fmap . fmap) (++) (genRegExpString r1) `composeAp` genRegExpString r2
genRegExpString (Alt r1 r2) = case (genRegExpString r1, genRegExpString r2) of
  (Nothing, x) -> x
  (x, Nothing) -> x
  (Just a, Just b) -> Just (oneof [a, b])
genRegExpString (Mark _ r) = genRegExpString r
genRegExpString (Star r) = case genRegExpString r of
  Nothing -> Nothing
  (Just xs) -> Just $ fmap (take 10 . concat) $ listOf xs

(%==%) :: RegExp -> RegExp -> Property
r1 %==% r2 = forAll (genString r1 r2) $
  \s -> match r1 s == match r2 s
  where
    genString :: RegExp -> RegExp -> Gen String
    genString r1s r2s = do
      xs <-
        oneof $
          catMaybes
            [ genRegExpString r1s,
              genRegExpString r2s,
              Just $ resize 10 (listOf (elements "abcd"))
            ]
      return xs

prop_rStar :: RegExp -> Property
prop_rStar r = prop_rStar1 r .&&. prop_rStar2 .&&. prop_rStar3
  where
    prop_rStar1 r = rStar (Star r) %==% Star (Star r)
    prop_rStar2 = rStar Empty %==% Star Empty
    prop_rStar3 = rStar Void %==% Star Empty

instance Arbitrary RegExp where
  arbitrary =
    frequency
      [ (1, return Void),
        (1, return Empty),
        (2, Star <$> arbitrary),
        (2, Char . Set.fromList <$> sublistOf ['a' .. 'z']),
        (2, Seq <$> arbitrary <*> arbitrary),
        (2, Alt <$> arbitrary <*> arbitrary)
      ]

--shrink = undefined

rSeq :: RegExp -> RegExp -> RegExp
rSeq Void x = Void
rSeq x Void = Void
rSeq Empty x = x
rSeq x Empty = x
rSeq a b = a `Seq` b

rAlt :: RegExp -> RegExp -> RegExp
rAlt l x | isVoid l = x
rAlt x r | isVoid r = x
rAlt Empty (Star r) = Star r
rAlt (Star r) Empty = Star r
rAlt a b | a == b = a
rAlt a b = a `Alt` b

-- notes from new version of the course

prop_seq :: RegExp -> RegExp -> Property
prop_seq r1 r2 = rs /= Seq r1 r2 ==> rs %==% Seq r1 r2
  where
    rs = rSeq r1 r2

prop_alt :: RegExp -> RegExp -> Property
prop_alt r1 r2 = rs /= Alt r1 r2 ==> rs %==% Alt r1 r2
  where
    rs = rAlt r1 r2

isEmpty :: RegExp -> Bool
isEmpty Empty = True
isEmpty (Seq r1 r2) = isEmpty r1 && isEmpty r2
isEmpty (Alt r1 r2) =
  (isEmpty r1 && isEmpty r2)
    || (isEmpty r1 && isVoid r2)
    || (isEmpty r2 && isVoid r1)
isEmpty (Star r) = isEmpty r || isVoid r
isEmpty (Char _) = False
isEmpty Void = False

isVoid :: RegExp -> Bool
isVoid Void = True
isVoid (Seq r1 r2) = isVoid r1 || isVoid r2
isVoid (Alt r1 r2) = isVoid r1 && isVoid r2
isVoid (Star _) = False -- can always match the empty string
isVoid (Char m) = Set.size m == 0
isVoid Empty = False